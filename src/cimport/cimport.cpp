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
#include "cimport.h"

#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
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

struct fcn_decl
{
    string name;
};

static vector<fcn_decl> functions;

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

    void declareFunction (NamedDecl *decl)
    {
        const FunctionDecl *fdecl = decl->getAsFunction();
        assert(NULL != fdecl);
        functions.push_back({ decl->getQualifiedNameAsString() });
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

int cimport_file (const char *filename)
{
    int argc = 3;
    const char *argv[] = { "", filename, "--" };
    cl::OptionCategory option_category("options");
    CommonOptionsParser options_parser(argc, argv, option_category);
    ClangTool tool(options_parser.getCompilations(),
                   options_parser.getSourcePathList());
    int ret = tool.run(newFrontendActionFactory<DeclFindingAction>().get());

    for (fcn_decl &fcn : functions) {
        outs() << fcn.name << "\n";
    }

    return ret;
}
