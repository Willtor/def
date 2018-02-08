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
#include <string>
#include <sstream>
#include <vector>

using namespace clang;
using namespace clang::tooling;
using namespace llvm;
using namespace std;

static vector<value> cvalues;

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
                declareStruct(decl);
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
        value pos = caml_alloc(4, 0);
        Store_field(pos, 0,
                    caml_copy_string(sm.getFilename(src_loc).str().c_str()));
        Store_field(pos, 1, Val_int(sm.getSpellingLineNumber(src_loc)));
        unsigned int file_offset = sm.getFileOffset(src_loc);
        unsigned int column = sm.getSpellingColumnNumber(src_loc);
        Store_field(pos, 2, Val_int(file_offset + 1 - column));
        Store_field(pos, 3, Val_int(file_offset));
        return pos;
    }

    value readType (QualType fulltype, SourceLocation loc)
    {
        // ---
        // FIXME: Shouldn't get rid of qualifications.  Need to deal with them.
        QualType qtype = fulltype.getUnqualifiedType();
        // ---

        const Type *type = qtype.getTypePtr();

        if (type->isPointerType()) {
            value pointee = readType(type->getPointeeType(), loc);
            value pos = position_of_SourceLocation(loc);
            value ret = caml_alloc(2, 1);
            Store_field(ret, 0, pos);
            Store_field(ret, 1, pointee);
            return ret;
        } else {
            value tname = caml_copy_string(qtype.getAsString().c_str());
            value pos = position_of_SourceLocation(loc);
            value ret = caml_alloc(2, 0);
            Store_field(ret, 0, pos);
            Store_field(ret, 1, tname);
            return ret;
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

    void declareStruct (NamedDecl *decl)
    {
        std::string name = decl->getNameAsString();

        if (name == "") {
            // FIXME: structs without names are typically part of a typedecl
            // and we should find a way to deal with this.
            return;
        }
        value pos = position_of_SourceLocation(decl->getLocStart());
        value sname = caml_copy_string(name.c_str());
        value type_opt;

        RecordDecl *record = static_cast<RecordDecl*>(decl);
        RecordDecl *defn = record->getDefinition();
        if (NULL != defn) {
            // Read the record's fields.
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
            value struct_def = caml_alloc(1, 3);
            Store_field(struct_def, 0, reverse_list(fields));
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
