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

//////////////////////////
#include "llvm/Support/CommandLine.h"
//////////////////////////

using namespace clang;
using namespace clang::tooling;
using namespace llvm;
using namespace std;

class DeclVisitor : public clang::RecursiveASTVisitor<DeclVisitor> {
protected:
    SourceManager &sm;

public:
    DeclVisitor (SourceManager &manager) : sm(manager) {}

    bool VisitNamedDecl (NamedDecl *decl) {
        outs() << "Found " << decl->getQualifiedNameAsString() << " at "
               << getDeclLocation(decl->getLocStart()) << "\n";
        return true;
    }

private:
    string getDeclLocation (SourceLocation loc) const {
        ostringstream oss;
        oss << sm.getFilename(loc).str() << ":"
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

// Apply a custom category to all command-line options so that they are the
// only ones displayed.
static cl::OptionCategory MyToolCategory("my-tool options");

// CommonOptionsParser declares HelpMessage with a description of the common
// command-line options related to the compilation database and input files.
// It's nice to have this help message in all tools.
static cl::extrahelp CommonHelp(CommonOptionsParser::HelpMessage);

// A help message for this specific tool can be added afterwards.
static cl::extrahelp MoreHelp("\nMore help text...");

int cimport_run (int argc, const char **argv)
{
    CommonOptionsParser OptionsParser(argc, argv, MyToolCategory);
    ClangTool Tool(OptionsParser.getCompilations(),
                   OptionsParser.getSourcePathList());
    int ret = Tool.run(newFrontendActionFactory<DeclFindingAction>().get());
    return ret;
}
