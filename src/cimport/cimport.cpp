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
#include <string>
#include <sstream>

using namespace clang;
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

unique_ptr<ASTConsumer>
DeclFindingAction::CreateASTConsumer (CompilerInstance &ci, StringRef)
{
    return unique_ptr<ASTConsumer>(new DeclFinder(ci.getSourceManager()));
}
