/* Copyright (C) 2018  DEF Authors

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 2
   of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
   02110-1301, USA.
 */

// Based on the Xinhuang Clang FindDecl tutorial at:
// https://github.com/xinhuang/clang-playground

#include <assert.h>

#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/alloc.h"
#include "caml/custom.h"
#include "caml/fail.h"

#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
#include <map>
#include <string>
#include <sstream>
#include <vector>

using namespace clang;
using namespace clang::tooling;
using namespace llvm;
using namespace std;

static vector<value> cvalues;
static map<SourceLocation, string> fund_types;

static value reverse_list (value list)
{
    CAMLparam1 (list);
    CAMLlocal1 (reversed_list);
    reversed_list = Val_int(0);
    while (list != Val_int(0)) {
        CAMLlocal1 (cdr);
        cdr = Field(list, 1);
        Store_field(list, 1, reversed_list);
        reversed_list = list;
        list = cdr;
    }
    CAMLreturn(reversed_list);
}

class DeclVisitor : public clang::RecursiveASTVisitor<DeclVisitor> {
protected:
    SourceManager &sm;

public:
    DeclVisitor (SourceManager &manager) : sm(manager) {}

    bool VisitNamedDecl (NamedDecl *decl)
    {
        // NOTE: Relevant kinds:
        // Function
        // Enum
        // Record (struct)
        // Field (member)
        // Typedef
        // Var
        // ParmVar (parameter)

        DeclContext *ctx = decl->getDeclContext();
        if (ctx->isFileContext() || ctx->isRecord()) {
            switch (decl->getKind()) {
            case Decl::Function:
                declareFunction(decl);
                break;
            case Decl::Record:
                declareRecord(decl);
                break;
            case Decl::Typedef:
                declareTypedef(decl);
                break;
            case Decl::Var:
                declareVariable(decl);
                break;
            default: break;
            }
        }

        return true;
    }

private:
    /** Lexing.position
     *  type position = {
     *      pos_fname : string;
     *      pos_lnum : int;
     *      pos_bol : int;
     *      pos_cnum : int;
     *  }
     */
    value position_of_SourceLocation (const SourceLocation src_loc)
    {
        CAMLparam0 ();

        SourceLocation spelling_loc = sm.getSpellingLoc(src_loc);
        unsigned int file_offset = sm.getFileOffset(spelling_loc);
        unsigned int column = sm.getSpellingColumnNumber(src_loc) - 1;

        CAMLlocal2 (pos, ocamlstr);
        pos = caml_alloc(4, 0);
        ocamlstr = caml_copy_string(sm.getFilename(spelling_loc).str()
                                    .c_str());

        Store_field(pos, 0, ocamlstr);
        Store_field(pos, 1, Val_int(sm.getSpellingLineNumber(src_loc)));
        Store_field(pos, 2, Val_int(file_offset - column));
        Store_field(pos, 3, Val_int(file_offset));

        CAMLreturn(pos);
    }

    value readFundamentalType (QualType qtype, SourceLocation loc)
    {
        CAMLparam0 ();

        string t(qtype.getAsString());
        fund_types[loc] = t;

        CAMLlocal3 (tname, pos, ret);
        tname = caml_copy_string(t.c_str());
        pos = position_of_SourceLocation(loc);
        ret = caml_alloc(2, 0);
        Store_field(ret, 0, pos);
        Store_field(ret, 1, tname);

        CAMLreturn(ret);
    }

    value readType (QualType fulltype, SourceLocation loc)
    {
        CAMLparam0 ();

        // ---
        // FIXME: Shouldn't get rid of qualifications.  Need to deal with them.
        QualType qtype = fulltype.getUnqualifiedType();
        // ---

        const Type *type = qtype.getTypePtr();

        if (type->isPointerType()) {
            // CT_Pointer
            CAMLlocal3 (pointee, pos, ret);
            pointee = readType(type->getPointeeType(), loc);
            pos = position_of_SourceLocation(loc);
            ret = caml_alloc(2, 1);
            Store_field(ret, 0, pos);
            Store_field(ret, 1, pointee);
            CAMLreturn(ret);
        } else if (type->isFunctionNoProtoType()) {
            // FIXME: don't know what to do with this.
            outs() <<
                "internal error: Hit a FunctionNoProtoType in cimport.cpp.\n";
            exit(125);
        } else if (type->isFunctionProtoType()) {
            // CT_Function
            const FunctionProtoType *ftype = type->castAs<FunctionProtoType>();

            CAMLlocal1 (param_list);
            param_list = Val_int(0);
            for (const QualType param : ftype->param_types()) {
                CAMLlocal1 (tmp);
                tmp = caml_alloc(2, 0);
                Store_field(tmp, 0, readType(param, loc));
                Store_field(tmp, 1, param_list);
                param_list = tmp;
            }
            param_list = reverse_list(param_list);

            value pos = position_of_SourceLocation(loc);
            value rettype = readType(ftype->getReturnType(), loc);
            value prototype = caml_alloc(4, 3);
            Store_field(prototype, 0, pos);
            Store_field(prototype, 1, param_list);
            Store_field(prototype, 2, Val_int(ftype->isVariadic()));
            Store_field(prototype, 3, rettype);
            CAMLreturn(prototype);
        } else if (type->isArrayType()) {
            // CT_Array

            string asString = qtype.getAsString();
            if (asString == "__builtin_va_list"
                || asString == "__gnuc_va_list") {
                // Certain types are listed as arrays of themselves.  These
                // are typically types that have some builtin meaning -
                // language hacks.  Treat them as fundamental types.
                CAMLreturn(readFundamentalType(qtype, loc));
            }

            CAMLlocal1 (pos);
            pos = position_of_SourceLocation(loc);
            if (type->isConstantArrayType()) {
                const ConstantArrayType *atype =
                    static_cast<const ConstantArrayType*>(type);
                CAMLlocal2 (subtype, ret);
                subtype = readType(atype->getElementType(), loc);
                ret = caml_alloc(3, 4);
                Store_field(ret, 0, pos);
                Store_field(ret, 1, subtype);
                Store_field(ret, 2, Val_int(atype->getSize().getZExtValue()));
                CAMLreturn(ret);
            } else if (type->isIncompleteArrayType()) {
                const IncompleteArrayType *atype =
                    static_cast<const IncompleteArrayType*>(type);
                CAMLlocal2 (subtype, ret);
                subtype = readType(atype->getElementType(), loc);
                ret = caml_alloc(3, 4);
                Store_field(ret, 0, pos);
                Store_field(ret, 1, subtype);
                Store_field(ret, 2, Val_int(0));
                CAMLreturn(ret);
            } else {
                outs() << "Internal error: unhandled array type.\n";
                exit(1);
            }
        } else {
            // CT_TypeName
            CAMLreturn(readFundamentalType(qtype, loc));
        }
    }

    void declareFunction (NamedDecl *decl)
    {
        CAMLparam0 ();

        const FunctionDecl *fdecl = decl->getAsFunction();

        CAMLlocal3 (pos, fname, param_list);
        pos = position_of_SourceLocation(fdecl->getLocStart());
        fname = caml_copy_string(fdecl->getQualifiedNameAsString().c_str());

        // Read the parameters into a list.
        param_list = Val_int(0);
        for (const ParmVarDecl *param : fdecl->parameters()) {
            CAMLlocal2 (param_n, tmp);
            param_n = readType(param->getOriginalType(),
                               param->getLocStart());
            tmp = caml_alloc(2, 0);
            Store_field(tmp, 0, param_n);
            Store_field(tmp, 1, param_list);
            param_list = tmp;
        }
        param_list = reverse_list(param_list);

        // Function's return type.
        CAMLlocal2 (rettp, is_variadic);
        rettp = readType(fdecl->getReturnType(),
                         fdecl->getReturnTypeSourceRange().getBegin());
        is_variadic = Val_bool(fdecl->isVariadic());

        // CV_Function of string * ctype list * ctype
        CAMLlocal1 (fcn);
        fcn = caml_alloc(5, 0);
        Store_field(fcn, 0, pos);
        Store_field(fcn, 1, fname);
        Store_field(fcn, 2, param_list);
        Store_field(fcn, 3, is_variadic);
        Store_field(fcn, 4, rettp);

        // ESCAPE
        cvalues.push_back(fcn);

        CAMLreturn0;
    }

    void declareRecord (NamedDecl *decl)
    {
        CAMLparam0 ();

        string name = decl->getNameAsString();
        SourceLocation loc = decl->getLocStart();

        if (name == "") {
            name = fund_types[loc];
            if (name == "") {
                // FIXME: getting a record shouldn't be this difficult,
                // and I must be doing something wrong, here.

                // For now, ignore records like this.
                CAMLreturn0;
            }
        }
        CAMLlocal3 (pos, sname, type_opt);
        pos = position_of_SourceLocation(loc);
        sname = caml_copy_string(name.c_str());

        RecordDecl *record = static_cast<RecordDecl*>(decl);
        RecordDecl *defn = record->getDefinition();
        if (NULL != defn) {
            // Read the record's fields.
            CAMLlocal2 (rec_kind, fields);
            rec_kind = Val_int(defn->getTypeForDecl()->isUnionType() ? 1 : 0);
            fields = Val_int(0);
            for (const FieldDecl *field : defn->fields()) {
                CAMLlocal5 (fpos, fname, ftype, frec, tmp);
                fpos = position_of_SourceLocation(field->getLocStart());
                fname = caml_copy_string(field->getNameAsString().c_str());
                ftype = readType(field->getType(), field->getLocStart());

                frec = caml_alloc(3, 2);
                Store_field(frec, 0, fpos);
                Store_field(frec, 1, fname);
                Store_field(frec, 2, ftype);

                tmp = fields;
                fields = caml_alloc(2, 0);
                Store_field(fields, 0, frec);
                Store_field(fields, 1, tmp);
            }
            CAMLlocal1 (struct_def);
            struct_def = caml_alloc(2, 2);
            Store_field(struct_def, 0, rec_kind);
            Store_field(struct_def, 1, reverse_list(fields));
            type_opt = caml_alloc(1, 0); // OCaml: Some t
            Store_field(type_opt, 0, struct_def);
        } else {
            // No definition for this record.
            type_opt = Val_int(0); // OCaml: None
        }
        CAMLlocal1 (sdecl);
        sdecl = caml_alloc(3, 1);
        Store_field(sdecl, 0, pos);
        Store_field(sdecl, 1, sname);
        Store_field(sdecl, 2, type_opt);
        // ESCAPE
        cvalues.push_back(sdecl);

        CAMLreturn0;
    }

    void declareTypedef (NamedDecl *decl)
    {
        CAMLparam0 ();
        SourceLocation src_loc = decl->getLocStart();
        CAMLlocal3 (pos, tname, t);
        pos = position_of_SourceLocation(src_loc);
        tname = caml_copy_string(decl->getNameAsString().c_str());

        TypedefNameDecl *typedefdecl = static_cast<TypedefNameDecl*>(decl);
        t = readType(typedefdecl->getUnderlyingType(), src_loc);

        // OCaml: Some type
        CAMLlocal2 (SomeT, tdecl);
        SomeT = caml_alloc(1, 0);
        Store_field(SomeT, 0, t);

        tdecl = caml_alloc(3, 1);
        Store_field(tdecl, 0, pos);
        Store_field(tdecl, 1, tname);
        Store_field(tdecl, 2, SomeT);
        // ESCAPE
        cvalues.push_back(tdecl);

        CAMLreturn0;
    }

    void declareVariable (NamedDecl *decl)
    {
        CAMLparam0 ();
        SourceLocation src_loc = decl->getLocStart();
        CAMLlocal4 (pos, vname, vtype, vdecl);
        pos = position_of_SourceLocation(src_loc);
        vname = caml_copy_string(decl->getNameAsString().c_str());
        ValueDecl *valuedecl = static_cast<ValueDecl*>(decl);
        vtype = readType(valuedecl->getType(), src_loc);

        vdecl = caml_alloc(4, 2);
        Store_field(vdecl, 0, pos);
        Store_field(vdecl, 1, vname);
        Store_field(vdecl, 2, vtype);
        Store_field(vdecl, 3, Val_bool(decl->hasExternalFormalLinkage()));
        // ESCAPE
        cvalues.push_back(vdecl);

        CAMLreturn0;
    }
};

class DeclFinder : public ASTConsumer
{
protected:
    DeclVisitor visitor;

public:
    DeclFinder(SourceManager &sm) : visitor(sm) {}

    void HandleTranslationUnit (ASTContext &context) final
    {
        visitor.TraverseDecl(context.getTranslationUnitDecl());
    }
};

class DeclFindingAction : public clang::ASTFrontendAction
{
public:
    std::unique_ptr<clang::ASTConsumer>
    CreateASTConsumer (clang::CompilerInstance &ci, StringRef) final
    {
        return unique_ptr<ASTConsumer>(new DeclFinder(ci.getSourceManager()));
    }
};

///////////////////////////////////////////////////////////////////////////////

value cimport_file (const char *filename, int argc, const char **argv)
{
    CAMLparam0();
    std::vector<std::string> source_files;
    source_files.push_back(filename);

    cl::OptionCategory option_category("options");
    CommonOptionsParser options_parser(argc, argv, option_category);
    ClangTool tool(options_parser.getCompilations(), source_files);
    if (0 != tool.run(newFrontendActionFactory<DeclFindingAction>().get())) {
        caml_failwith("");
        // Shouldn't make it here.
        abort();
    }

    // Build the list of cvalues.
    CAMLlocal1 (cv_list);
    cv_list = Val_int(0);
    for (vector<value>::iterator cvi = cvalues.begin();
         cvi != cvalues.end();
         ++cvi) {
        CAMLlocal1 (tmp);
        tmp = caml_alloc(2, 0);
        Store_field(tmp, 0, *cvi);
        Store_field(tmp, 1, cv_list);
        cv_list = tmp;
    }
    cv_list = reverse_list(cv_list);

    // Empty the cvalues and fund_types sets in case this function gets called
    // again.  It would be "very bad" to have stale data lying around.
    // ESCAPE?
    cvalues.clear();
    // ESCAPE?
    fund_types.clear();

    CAMLreturn(cv_list);
}

/** import_c_file f p: Import the C header file, f, using the include
 *  paths, p, and return a set of type and function declarations.
 */
extern "C"
CAMLprim value cimport_import_c_file (value filename, value paths)
{
    CAMLparam2 (filename, paths);
    char *file = String_val(filename);

    std::vector<std::string> path_vec;
    for ( ; paths != Val_int(0); paths = Field(paths, 1)) {
        // ESCAPE?
        path_vec.push_back(std::string("-I") + String_val(Field(paths, 0)));
    }
    path_vec.push_back(std::string("-D__def=1"));
    int argc = path_vec.size() + 3;
    const char *argv[argc];
    argv[0] = "def";
    argv[1] = file;
    argv[2] = "--";
    for (int i = 3; i < argc; ++i) {
        argv[i] = path_vec[i - 3].c_str();
    }

    CAMLreturn(cimport_file(file, argc, argv));
}
