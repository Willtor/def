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
    value reversed_list = Val_int(0);
    while (list != Val_int(0)) {
        value cdr = Field(list, 1);
        Store_field(list, 1, reversed_list);
        reversed_list = list;
        list = cdr;
    }
    return reversed_list;
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
        SourceLocation spelling_loc = sm.getSpellingLoc(src_loc);
        unsigned int file_offset = sm.getFileOffset(spelling_loc);
        unsigned int column = sm.getSpellingColumnNumber(src_loc) - 1;

        value pos = caml_alloc(4, 0);
        Store_field(pos, 0,
                    caml_copy_string(sm.getFilename(spelling_loc).str()
                                     .c_str()));
        Store_field(pos, 1, Val_int(sm.getSpellingLineNumber(src_loc)));
        Store_field(pos, 2, Val_int(file_offset - column));
        Store_field(pos, 3, Val_int(file_offset));

        return pos;
    }

    value readFundamentalType (QualType qtype, SourceLocation loc)
    {
        string t = qtype.getAsString();
        fund_types[loc] = t;
        value tname = caml_copy_string(t.c_str());
        value pos = position_of_SourceLocation(loc);
        value ret = caml_alloc(2, 0);
        Store_field(ret, 0, pos);
        Store_field(ret, 1, tname);
        return ret;
    }

    value readType (QualType fulltype, SourceLocation loc)
    {
        // ---
        // FIXME: Shouldn't get rid of qualifications.  Need to deal with them.
        QualType qtype = fulltype.getUnqualifiedType();
        // ---

        const Type *type = qtype.getTypePtr();

        if (type->isPointerType()) {
            // CT_Pointer
            value pointee = readType(type->getPointeeType(), loc);
            value pos = position_of_SourceLocation(loc);
            value ret = caml_alloc(2, 1);
            Store_field(ret, 0, pos);
            Store_field(ret, 1, pointee);
            return ret;
        } else if (type->isFunctionNoProtoType()) {
            // FIXME: don't know what to do with this.
            outs() <<
                "internal error: Hit a FunctionNoProtoType in cimport.cpp.\n";
            exit(125);
        } else if (type->isFunctionProtoType()) {
            // CT_Function
            const FunctionProtoType *ftype = type->castAs<FunctionProtoType>();

            value param_list = Val_int(0);
            for (const QualType param : ftype->param_types()) {
                value tmp = caml_alloc(2, 0);
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
            return prototype;
        } else if (type->isArrayType()) {
            // CT_Array

            string asString = qtype.getAsString();
            if (asString == "__builtin_va_list"
                || asString == "__gnuc_va_list") {
                // Certain types are listed as arrays of themselves.  These
                // are typically types that have some builtin meaning -
                // language hacks.  Treat them as fundamental types.
                return readFundamentalType(qtype, loc);
            }

            value pos = position_of_SourceLocation(loc);
            if (type->isConstantArrayType()) {
                const ConstantArrayType *atype =
                    static_cast<const ConstantArrayType*>(type);
                value subtype = readType(atype->getElementType(), loc);
                value ret = caml_alloc(3, 4);
                Store_field(ret, 0, pos);
                Store_field(ret, 1, subtype);
                Store_field(ret, 2, Int_val(atype->getSize().getZExtValue()));
                return ret;
            } else if (type->isIncompleteArrayType()) {
                const IncompleteArrayType *atype =
                    static_cast<const IncompleteArrayType*>(type);
                value subtype = readType(atype->getElementType(), loc);
                value ret = caml_alloc(3, 4);
                Store_field(ret, 0, pos);
                Store_field(ret, 1, subtype);
                Store_field(ret, 2, Int_val(0));
                return ret;
            } else {
                outs() << "Internal error: unhandled array type.\n";
                exit(1);
            }
        } else {
            // CT_TypeName
            return readFundamentalType(qtype, loc);
        }
    }

    void declareFunction (NamedDecl *decl)
    {
        const FunctionDecl *fdecl = decl->getAsFunction();

        value pos = position_of_SourceLocation(fdecl->getLocStart());
        value fname =
            caml_copy_string(fdecl->getQualifiedNameAsString().c_str());

        // Read the parameters into a list.
        value param_list = Val_int(0);
        for (const ParmVarDecl *param : fdecl->parameters()) {
            value param_n = readType(param->getOriginalType(),
                                     param->getLocStart());
            value tmp = caml_alloc(2, 0);
            Store_field(tmp, 0, param_n);
            Store_field(tmp, 1, param_list);
            param_list = tmp;
        }
        param_list = reverse_list(param_list);

        // Function's return type.
        value rettp = readType(fdecl->getReturnType(),
                               fdecl->getReturnTypeSourceRange().getBegin());
        value is_variadic = Val_bool(fdecl->isVariadic());

        // CV_Function of string * ctype list * ctype
        value fcn = caml_alloc(5, 0);
        Store_field(fcn, 0, pos);
        Store_field(fcn, 1, fname);
        Store_field(fcn, 2, param_list);
        Store_field(fcn, 3, is_variadic);
        Store_field(fcn, 4, rettp);

        cvalues.push_back(fcn);
    }

    void declareRecord (NamedDecl *decl)
    {
        string name = decl->getNameAsString();
        SourceLocation loc = decl->getLocStart();

        if (name == "") {
            name = fund_types[loc];
            if (name == "") {
                // FIXME: getting a record shouldn't be this difficult,
                // and I must be doing something wrong, here.

                // For now, ignore records like this.
                return;
            }
        }
        value pos = position_of_SourceLocation(loc);
        value sname = caml_copy_string(name.c_str());
        value type_opt;

        RecordDecl *record = static_cast<RecordDecl*>(decl);
        RecordDecl *defn = record->getDefinition();
        if (NULL != defn) {
            // Read the record's fields.
            value rec_kind = Val_int(defn->getTypeForDecl()->isUnionType()
                                     ? 1 : 0);
            value fields = Val_int(0);
            for (const FieldDecl *field : defn->fields()) {
                value fpos = position_of_SourceLocation(field->getLocStart());
                value fname =
                    caml_copy_string(field->getNameAsString().c_str());
                value ftype = readType(field->getType(),
                                       field->getLocStart());

                value frec = caml_alloc(3, 2);
                Store_field(frec, 0, fpos);
                Store_field(frec, 1, fname);
                Store_field(frec, 2, ftype);

                value tmp = fields;
                fields = caml_alloc(2, 0);
                Store_field(fields, 0, frec);
                Store_field(fields, 1, tmp);
            }
            value struct_def = caml_alloc(2, 2);
            Store_field(struct_def, 0, rec_kind);
            Store_field(struct_def, 1, reverse_list(fields));
            type_opt = caml_alloc(1, 0); // OCaml: Some t
            Store_field(type_opt, 0, struct_def);
        } else {
            // No definition for this record.
            type_opt = Val_int(0); // OCaml: None
        }
        value sdecl = caml_alloc(3, 1);
        Store_field(sdecl, 0, pos);
        Store_field(sdecl, 1, sname);
        Store_field(sdecl, 2, type_opt);
        cvalues.push_back(sdecl);
    }

    void declareTypedef (NamedDecl *decl)
    {
        SourceLocation src_loc = decl->getLocStart();
        value pos = position_of_SourceLocation(src_loc);
        value tname = caml_copy_string(decl->getNameAsString().c_str());

        TypedefNameDecl *typedefdecl = static_cast<TypedefNameDecl*>(decl);
        value t = readType(typedefdecl->getUnderlyingType(), src_loc);

        // OCaml: Some type
        value SomeT = caml_alloc(1, 0);
        Store_field(SomeT, 0, t);

        value tdecl = caml_alloc(3, 1);
        Store_field(tdecl, 0, pos);
        Store_field(tdecl, 1, tname);
        Store_field(tdecl, 2, SomeT);
        cvalues.push_back(tdecl);
    }

    void declareVariable (NamedDecl *decl)
    {
        SourceLocation src_loc = decl->getLocStart();
        value pos = position_of_SourceLocation(src_loc);
        value vname = caml_copy_string(decl->getNameAsString().c_str());
        ValueDecl *valuedecl = static_cast<ValueDecl*>(decl);
        value vtype = readType(valuedecl->getType(), src_loc);

        value vdecl = caml_alloc(4, 2);
        Store_field(vdecl, 0, pos);
        Store_field(vdecl, 1, vname);
        Store_field(vdecl, 2, vtype);
        Store_field(vdecl, 3, Val_bool(decl->hasExternalFormalLinkage()));
        cvalues.push_back(vdecl);
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
    std::vector<std::string> source_files;
    source_files.push_back(filename);

    cl::OptionCategory option_category("options");
    CommonOptionsParser options_parser(argc, argv, option_category);
    ClangTool tool(options_parser.getCompilations(), source_files);
    if (0 != tool.run(newFrontendActionFactory<DeclFindingAction>().get())) {
        // FIXME: Should throw an exception.
        outs() << "### BAD EXIT FROM C FILE.\n";
    }

    // Build the list of cvalues.
    value cv_list = Val_int(0);
    for (value cv : cvalues) {
        value tmp = caml_alloc(2, 0);
        Store_field(tmp, 0, cv);
        Store_field(tmp, 1, cv_list);
        cv_list = tmp;
    }
    cv_list = reverse_list(cv_list);

    return cv_list;
}

/** import_c_file f p: Import the C header file, f, using the include
 *  paths, p, and return a set of type and function declarations.
 */
extern "C"
CAMLprim value cimport_import_c_file (value filename, value paths)
{
    char *file = String_val(filename);

    std::vector<std::string> path_vec;
    for ( ; paths != Val_int(0); paths = Field(paths, 1)) {
        path_vec.push_back(std::string("-I") + String_val(Field(paths, 0)));
    }
    int argc = path_vec.size() + 3;
    const char *argv[argc];
    argv[0] = "def";
    argv[1] = file;
    argv[2] = "--";
    for (int i = 3; i < argc; ++i) {
        argv[i] = path_vec[i - 3].c_str();
    }

    return cimport_file(file, argc, argv);
}
