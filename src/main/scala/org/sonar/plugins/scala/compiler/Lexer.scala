/*
 * Sonar Scala Plugin
 * Copyright (C) 2011 - 2014 All contributors
 * dev@sonar.codehaus.org
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02
 */
package org.sonar.plugins.scala.compiler

import collection.mutable.ListBuffer

import org.sonar.plugins.scala.language.{Comment, CommentType}
import scala.reflect.io.AbstractFile
import scala.reflect.internal.util.BatchSourceFile
import scala.tools.nsc.util.{CharArrayReader, CharArrayReaderData}

/**
 * This class is a wrapper for accessing the lexer of the Scala compiler
 * from Java in a more convenient way.
 *
 * @author Felix Müller
 * @since 0.1
 */
class Lexer {

  import scala.collection.JavaConversions._
  import Compiler._

  def getTokens(code: String): java.util.List[Token] = {
    val unit = new CompilationUnit(new BatchSourceFile("", code.toCharArray))
    tokenize(unit)
  }

  def getTokensOfFile(path: String): java.util.List[Token] = {
    val unit = new CompilationUnit(new BatchSourceFile(AbstractFile.getFile(path)))
    tokenize(unit)
  }

  private def tokenize(unit: CompilationUnit): java.util.List[Token] = {
    val scanner = new syntaxAnalyzer.UnitScanner(unit)
    val tokens = ListBuffer[Token]()

    scanner.init()
    while (scanner.token != scala.tools.nsc.ast.parser.Tokens.EOF) {
      tokens += Token(scanner.token, scanner.parensAnalyzer.line(scanner.offset) + 1)
      scanner.nextToken()
    }
    tokens
  }

  def getComments(code: String): java.util.List[Comment] = {
    val unit = new CompilationUnit(new BatchSourceFile("", code.toCharArray))
    tokenizeComments(unit)
  }

  def getCommentsOfFile(path: String): java.util.List[Comment] = {
    val unit = new CompilationUnit(new BatchSourceFile(AbstractFile.getFile(path)))
    tokenizeComments(unit)
  }

  private def tokenizeComments(unit: CompilationUnit): java.util.List[Comment] = {
    val comments = ListBuffer[Comment]()
    val scanner = new syntaxAnalyzer.UnitScanner(unit) {

    //  private var lastDocCommentRange: Option[Range] = None

    private var foundToken = false

    override def nextToken() {
      super.nextToken()
      foundToken = token != 0
    }

    override def skipComment(): Boolean = {
      val reply: Boolean = super.skipComment()
      val commentVal: String = source.content.mkString.substring(offset,lineStartOffset)
      var antecedeVal: String = ""
      if ( source.content.mkString.substring(lineStartOffset) != null ) {
        antecedeVal = source.content.mkString.substring(lineStartOffset)
      }

      def isHeaderComment(value: String) = {
        !foundToken && comments.isEmpty && value.trim().startsWith("/*") && !value.trim().startsWith("/**")
      }

      def precedesDeclaration(): Boolean = {
        /*
          doc comment is
          before a class, trait, or object declaration;
          before a package object declaration (note that comments are not meaningful before packagings, see reference $9.2);
          before a value member declaration (method, value, variable);
          before a type member declaration (alias and abstract types).
        */
        val declarations = List("class","trait","object","def","protected","private","final","val","var","int","alias","abstract","override","fun","public","lazy","@")
        val antecedeList = antecedeVal.trim.split("\\s+")
        val antecedeFirstWord = antecedeList(0)
        declarations.contains(antecedeFirstWord)
      }

      def precedesPrivateDeclaration(): Boolean = {
        val declarations = List("private")
        declarations.contains(antecedeVal)
      }

      def isDocComment(value: String) = {
        value.trim().startsWith("/**") && precedesDeclaration()
      }

      def isPublicDocComment(value: String) = {
        isDocComment(value) && ! precedesPrivateDeclaration()
      }

      def isPrivateDocComment(value: String) = {
        isDocComment(value) && precedesPrivateDeclaration()
      }

      if ( reply ) {
        if (isHeaderComment(commentVal)) {
          comments += new Comment(commentVal, CommentType.HEADER)
        } else {
          if (isPublicDocComment(commentVal)) {
            comments += new Comment(commentVal, CommentType.DOC)
          } else {
            if (isPrivateDocComment(commentVal)) {
              comments += new Comment(commentVal, CommentType.PRIVATEDOC)
            } else {
              comments += new Comment(commentVal, CommentType.NORMAL)
            }
          }
        }
      }
        reply
      }
    }

    scanner.init()
    while (scanner.token != scala.tools.nsc.ast.parser.Tokens.EOF) {
      scanner.nextToken()
    }

    comments
  }
}
