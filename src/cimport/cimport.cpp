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
#include <cstdlib>
#include <memory>
#include <string>
#include <sstream>
#include <vector>

//////////////////////////
#include "llvm/Support/CommandLine.h"
//////////////////////////

using namespace clang;
using namespace clang::tooling;
using namespace llvm;
using namespace std;

static vector<value> functions;

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

    const char *kindName (NamedDecl *decl)
    {
        switch (decl->getKind()) {
        case Decl::Function:
            return "Function";
        case Decl::Enum:
            return "Enum";
        case Decl::Record: // struct.
            return "Record";
        case Decl::Typedef:
            return "Typedef";
        case Decl::Field:
            return "Field";
        case Decl::Var:
            return "Var";
        case Decl::ParmVar:
            return "ParmVar";
        default:
            return "Other";
        }
    }

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

    value readType (QualType qtype, SourceLocation loc)
    {
        value tname = caml_copy_string(qtype.getAsString().c_str());
        value pos = position_of_SourceLocation(loc);
        value ret = caml_alloc(2, 0);
        Store_field(ret, 0, pos);
        Store_field(ret, 1, tname);
        return ret;
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

        // CV_Function of string * ctype list * ctype
        value fcn = caml_alloc(4, 0);
        Store_field(fcn, 0, pos);
        Store_field(fcn, 1, fname);
        Store_field(fcn, 2, param_list);
        Store_field(fcn, 3, rettp);

        functions.push_back(fcn);
    }

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
            default: break;
            }
            /*
            outs() << "Found " << decl->getQualifiedNameAsString() << " at "
                   << getDeclLocation(decl->getLocStart())
                   << "(" << kindName(decl) << ")\n";
            */
        }

        return true;
    }

private:
    string getDeclLocation (SourceLocation loc) const {
        ostringstream oss;
        oss /*<< sm.getFilename(loc).str() << ":"*/
            << sm.getSpellingLineNumber(loc) << ":"
            << sm.getSpellingColumnNumber(loc);
        return oss.str();
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

value cimport_file (const char *filename)
{
    int argc = 3;
    const char *argv[] = { "", filename, "--" };
    cl::OptionCategory option_category("options");
    CommonOptionsParser options_parser(argc, argv, option_category);
    ClangTool tool(options_parser.getCompilations(),
                   options_parser.getSourcePathList());
    if (0 != tool.run(newFrontendActionFactory<DeclFindingAction>().get())) {
        outs() << "### BAD EXIT FROM C FILE.\n";
    }

    // Build the list of functions.
    value fcn_list = Val_int(0);
    for (value fcn : functions) {
        value tmp = caml_alloc(2, 0);
        Store_field(tmp, 0, fcn);
        Store_field(tmp, 1, fcn_list);
        fcn_list = tmp;
    }
    fcn_list = reverse_list(fcn_list);

    return fcn_list;
}

extern "C"
CAMLprim value cimport_import_c_file (value filename)
{
    return cimport_file(String_val(filename));
}
