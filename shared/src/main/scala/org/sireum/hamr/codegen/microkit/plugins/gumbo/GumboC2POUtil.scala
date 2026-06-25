// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.gumbo

import org.sireum._
import org.sireum.hamr.codegen.common.symbols.GclSymbolTable

object GumboC2POUtil {

  @enum object C2POInputType {
    "API_REFERENCE"
    "API_REFERENCE_NONEMPTY"
    "API_REFERENCE_ISEMPTY"
    "LOCAL_VARIABLE_OR_STATE"
    "FUNCTION"
    // Add your new target here!
  }

  @enum object C2POType {
    "bool"
    "int"
    "float"
//    "struct"
//    "array"
  }

  @pure def getExprType(exp: org.sireum.lang.ast.Exp, gclSymbolTable: GclSymbolTable): C2POType.Type = {
    exp match {
      // 1. Literal Nodes have fixed concrete types
      case _: org.sireum.lang.ast.Exp.LitB => return C2POType.bool
      case _: org.sireum.lang.ast.Exp.LitZ => return C2POType.int
      case _: org.sireum.lang.ast.Exp.LitF32 => return C2POType.float // R2U2 supports f64 by default, f32 can be cast up to f64 within R2U2
      case _: org.sireum.lang.ast.Exp.LitF64 => return C2POType.float
      case _: org.sireum.lang.ast.Exp.LitR => return C2POType.float
      case _: org.sireum.lang.ast.Exp.LitString => halt("Strings are not supported by C2PO/R2U2")


      // 2. Logic and comparison operations always return standard Booleans
      case bin: org.sireum.lang.ast.Exp.Binary =>
        if ( // Relational operations
          bin.op == org.sireum.lang.ast.Exp.BinaryOp.Lt ||
          bin.op == org.sireum.lang.ast.Exp.BinaryOp.Le ||
          bin.op == org.sireum.lang.ast.Exp.BinaryOp.Gt ||
          bin.op == org.sireum.lang.ast.Exp.BinaryOp.Ge ||
          bin.op == org.sireum.lang.ast.Exp.BinaryOp.CondAnd ||
          bin.op == org.sireum.lang.ast.Exp.BinaryOp.CondOr ||
          bin.op == org.sireum.lang.ast.Exp.BinaryOp.CondImply ||
          bin.op == org.sireum.lang.ast.Exp.BinaryOp.Eq ||
          bin.op == org.sireum.lang.ast.Exp.BinaryOp.Ne
        ) {
          return C2POType.bool
        } else if ( // Bitwise operations
            bin.op == org.sireum.lang.ast.Exp.BinaryOp.And ||
            bin.op == org.sireum.lang.ast.Exp.BinaryOp.Or ||
            bin.op == org.sireum.lang.ast.Exp.BinaryOp.Xor ||
            bin.op == org.sireum.lang.ast.Exp.BinaryOp.Shl ||
            bin.op == org.sireum.lang.ast.Exp.BinaryOp.Shr
        ){
          // Need to infer int or bool math based on left side and right side
          val return_type_left = getExprType(bin.left, gclSymbolTable)
          val return_type_right = getExprType(bin.right, gclSymbolTable)
          if (return_type_left == return_type_right && (return_type_left == C2POType.int || return_type_left == C2POType.bool)){
            return return_type_left
          } else {
            halt("Expression type is not supported by C2PO/R2U2")
          }
        }
        else if ( // Arithmetic operations
          bin.op == org.sireum.lang.ast.Exp.BinaryOp.Add ||
            bin.op == org.sireum.lang.ast.Exp.BinaryOp.Sub ||
            bin.op == org.sireum.lang.ast.Exp.BinaryOp.Mul ||
            bin.op == org.sireum.lang.ast.Exp.BinaryOp.Div ||
            bin.op == org.sireum.lang.ast.Exp.BinaryOp.Rem
        ){
          // Need to infer int or float math based on left side and right side
          val return_type_left = getExprType(bin.left, gclSymbolTable)
          val return_type_right = getExprType(bin.right, gclSymbolTable)
          if (return_type_left == return_type_right && (return_type_left == C2POType.int || return_type_left == C2POType.float)){
            return return_type_left
          } else {
            halt("Expression type is not supported by C2PO/R2U2")
          }
        } else {
          halt("Expression type is not supported by C2PO/R2U2")
          // To-Do: These are the unclassified ones:
//          val Equiv: String = "==="
//          val EquivUni: String = "≡"
//          val Inequiv: String = "=!="
//          val InequivUni: String = "≢"
//          val FpEq: String = "~~"
//          val FpNe: String = "!~"
//          val Ushr: String = ">>>"
//          val Imply: String = "__>:"
//          val Append: String = ":+"
//          val Prepend: String = "+:"
//          val AppendAll: String = "++"
//          val RemoveAll: String = "--"
//          val MapsTo: String = "~>"
//          val Arrow: String = "=>:"
        }

      case un: org.sireum.lang.ast.Exp.Unary =>
        if ( // Relational operations
          un.op == org.sireum.lang.ast.Exp.UnaryOp.Not
        ) {
          return C2POType.bool
        } else if (
            un.op == org.sireum.lang.ast.Exp.UnaryOp.Complement
        ) { // Bitwise operations
          // Need to infer type based on right side (int or bool)
          val return_type = getExprType(un.exp, gclSymbolTable)
          if (return_type == C2POType.int || return_type == C2POType.bool){
            return return_type
          } else {
            halt("Expression type is not supported by C2PO/R2U2")
          }
        } else if ( // Arithmetic signs
          un.op == org.sireum.lang.ast.Exp.UnaryOp.Plus ||
          un.op == org.sireum.lang.ast.Exp.UnaryOp.Minus
        ) {
          // Need to infer type based on right side (int or float)
          val return_type = getExprType(un.exp, gclSymbolTable)
          if (return_type == C2POType.int || return_type == C2POType.float){
            return return_type
          } else {
            halt("Expression type is not supported by C2PO/R2U2")
          }
        } else {
          halt("Expression type is not supported by C2PO/R2U2")
        }
      // 3. Status checks on nested properties are structurally Booleans
      case sel: org.sireum.lang.ast.Exp.Select if
        sel.id.value == "nonEmpty" || sel.id.value == "isEmpty" =>
        return C2POType.bool

      // 4. Identifiers must be checked against your scope context
      case id: org.sireum.lang.ast.Exp.Ident =>
        halt("Expression type is not supported by C2PO/R2U2")
//        val name = id.id.value
//        // Query symbol table records for type details
//        // (Adjust this specific call to match your actual symbol table architecture)
//        val lookupType = gclSymbolTable.apiReferences.elements.find(api => api.identifier == name)
//        lookupType match {
//          case Some(apiRef) => halt("Expression type is not supported by C2PO/R2U2") // To-Do
//          case _ => halt("Expression type is not supported by C2PO/R2U2")        // Standard local variable fallback
//        }

      case _ => halt("Expression type is not supported by C2PO/R2U2")
    }
  }

  @pure def collectIdentifiers(exp: org.sireum.lang.ast.Exp,
                               gclSymbolTable: GclSymbolTable,
                               inner_ref: Option[C2POInputType.Type] = None()
                              ): (org.sireum.lang.ast.Exp, ISZ[(String, org.sireum.lang.ast.Exp, C2POInputType.Type)]) = {
    var categorized = ISZ[(String, org.sireum.lang.ast.Exp, C2POInputType.Type)]()
    // Helper closure to build a clean string chain ("api_myStructArray_nonEmpty")
    // It returns None() if the selection path does not originate from the 'api' object
    def getFlatPathString(subExp: org.sireum.lang.ast.Exp): Option[String] = {
      subExp match {
        case id: org.sireum.lang.ast.Exp.Ident if id.id.value == "api" =>
          return Some("api")

        case sel: org.sireum.lang.ast.Exp.Select =>
          sel.receiverOpt match {
            case Some(recv) =>
              getFlatPathString(recv) match {
                case Some(prefix) => return Some(s"${prefix}_${sel.id.value}")
                case _ => return None()
              }
            case _ => return None()
          }
        case _ => return None()
      }
    }

    exp match {
      // 1. Matches simple variables, ports, or standalone identifiers
      case id: org.sireum.lang.ast.Exp.Ident =>
        val name = id.id.value
        val isApi = gclSymbolTable.apiReferences.elements.exists(api => api.identifier == name)
        val tagStr = if (isApi) {
          inner_ref.getOrElse(C2POInputType.API_REFERENCE)
        } else {
          inner_ref.getOrElse(C2POInputType.LOCAL_VARIABLE_OR_STATE)
        }
        categorized = categorized :+ (name, id, tagStr)
        return (id, categorized)
      // 2. Matches component dot-selections (e.g., api.my_var or state.my_var)
      case sel: org.sireum.lang.ast.Exp.Select =>
        val propName = sel.id.value
        if (propName == "nonEmpty" || propName == "isEmpty") {
          var input_ref: C2POInputType.Type = C2POInputType.API_REFERENCE
          if (propName == "nonEmpty"){
            input_ref = C2POInputType.API_REFERENCE_NONEMPTY
          } else {
            input_ref = C2POInputType.API_REFERENCE_ISEMPTY
          }

          getFlatPathString(sel) match {
            case Some(collapsedString) =>
              println("We got here!");
              // 1A. SUCCESS: We found an API chain ending in a collection status!
              categorized = categorized :+ (collapsedString, sel, input_ref)

              // Generate and return a single fresh identifier node, eliminating the deep sub-tree
              val freshId = org.sireum.lang.ast.Id(value = collapsedString, attr = sel.id.attr)
              val rewrittenNode = org.sireum.lang.ast.Exp.Ident(id = freshId, attr = sel.attr)
              return (rewrittenNode, categorized)

            case _ =>
              // Fallback if it's a non-api selection ending in nonEmpty
              val (updatedRecv, innerMapping) = sel.receiverOpt match {
                case Some(r) =>
                  val res = collectIdentifiers(r, gclSymbolTable, inner_ref)
                  (Some(res._1), res._2)
                case _ => (None[org.sireum.lang.ast.Exp](), ISZ[(String, org.sireum.lang.ast.Exp, C2POInputType.Type)]())
              }
              return (sel(receiverOpt = updatedRecv), categorized ++ innerMapping)
          }
        } else {
          // Standard property tree execution path (like api.myStructArray)
          getFlatPathString(sel) match {
            case Some(collapsedString) =>
              // 1B. SUCCESS: We found a standard standalone API chain
              val isApi = gclSymbolTable.apiReferences.elements.exists(api => api.identifier == propName)
              val tagStr = if (isApi) {
                inner_ref.getOrElse(C2POInputType.API_REFERENCE)
              } else {
                inner_ref.getOrElse(C2POInputType.LOCAL_VARIABLE_OR_STATE)
              }
              categorized = categorized :+ (collapsedString, sel, tagStr)

              val freshId = org.sireum.lang.ast.Id(value = collapsedString, attr = sel.id.attr)
              val rewrittenNode = org.sireum.lang.ast.Exp.Ident(id = freshId, attr = sel.attr)
              return (rewrittenNode, categorized)

            case _ =>
              // Standard object configuration path, map inner nodes recursively
              val (updatedRecv, innerMapping) = sel.receiverOpt match {
                case Some(r) =>
                  val res = collectIdentifiers(r, gclSymbolTable, inner_ref)
                  (Some(res._1), res._2)
                case _ => (None[org.sireum.lang.ast.Exp](), ISZ[(String, org.sireum.lang.ast.Exp, C2POInputType.Type)]())
              }
              return (sel(receiverOpt = updatedRecv), categorized ++ innerMapping)
          }
        }
      // 3. Drill down into Binary Operators (e.g., x > 5, a AND b)
      case bin: org.sireum.lang.ast.Exp.Binary =>
        val res_left = collectIdentifiers(bin.left, gclSymbolTable)
        val res_right = collectIdentifiers(bin.right, gclSymbolTable)
        return (bin(res_left._1, bin.op, res_right._1), categorized ++ res_left._2 ++ res_right._2)
      // 4. Drill down into Unary Operators (e.g., !x)
      case un: org.sireum.lang.ast.Exp.Unary =>
        val res = collectIdentifiers(un.exp, gclSymbolTable)
        return (un(exp = res._1), categorized ++ res._2)
      // 5. Drill down into Function/Method invocations (e.g., compute(x, y))
      case invoke: org.sireum.lang.ast.Exp.Invoke =>
        println(s"This is an Invoke variable: ${invoke.ident.id.value}")
        val functionName: String = invoke.receiverOpt.getOrElse(invoke.ident) match {
          case fId: org.sireum.lang.ast.Exp.Ident => fId.id.value
          case _ => ""
        }
        if (functionName != "") {
          categorized = categorized :+ (functionName, invoke, C2POInputType.FUNCTION)
        }
        var updatedArgs = ISZ[org.sireum.lang.ast.Exp]()
        var accumulatedMapping = categorized

        for (arg <- invoke.args.elements) {
          val (newArg, argMapping) = collectIdentifiers(arg, gclSymbolTable, inner_ref)
          updatedArgs = updatedArgs :+ newArg
          accumulatedMapping = accumulatedMapping ++ argMapping
        }

        val res = invoke.receiverOpt match {
          case Some(r) =>
            val res_inner = collectIdentifiers(r, gclSymbolTable, inner_ref)
            (Some(res_inner._1), res_inner._2)
          case _ => (None[org.sireum.lang.ast.Exp](), ISZ[(String, org.sireum.lang.ast.Exp, C2POInputType.Type)]())
        }

        return (invoke(receiverOpt = res._1, args = updatedArgs), accumulatedMapping ++ res._2)
      // Fallback for literals, constants, or unsupported expressions
      case leaf =>
        return (leaf, categorized)
    }
  }
}