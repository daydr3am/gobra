package viper.gobra.frontend.info.implementation.typing

import viper.gobra.ast.frontend.{PActualOpApp, PCardinality, PDot, PExpression, PGhostOpApp, POpApp, AstPattern => ap}
import viper.gobra.frontend.info.base.Type._
import viper.gobra.frontend.info.base.{SymbolTable, TypeSubstitution}
import viper.gobra.frontend.info.implementation.TypeInfoImpl

trait OpSignature { this: TypeInfoImpl =>

  def ghostSignature(op: PGhostOpApp): Seq[TypeSubstitution] = op match {
    case _: PCardinality => Seq(
      Map(POpApp.pArg(0) -> SetT(elementType), POpApp.pRes -> IntT(config.typeBounds.UntypedConst)),
      Map(POpApp.pArg(0) -> MultisetT(elementType), POpApp.pRes -> IntT(config.typeBounds.UntypedConst))
    )

  }

  val elementType: FreeTypeVar = FreeTypeVar("E")

  def actualSignature(op: PActualOpApp): Seq[TypeSubstitution] = op match {
    case n: PDot => resolve(n).get match {
      case ap.AdtField(_, _, symb) => symb match {
        case SymbolTable.AdtDestructor(decl, adtType, _) =>
          Seq(Map(POpApp.pArg(0) -> typeSymbType(adtType), POpApp.pRes -> typeSymbType(decl.typ)))
        case SymbolTable.AdtDiscriminator(_, adtType, _) =>
          Seq(Map(POpApp.pArg(0) -> typeSymbType(adtType), POpApp.pRes -> BooleanT))
        case SymbolTable.AdtClauseField(_, _, _, _) => ???
      }
      case _ => ???
    }
    case _ => ???
  }

  def signature(op: POpApp): Seq[TypeSubstitution] = op match {
    case app: PGhostOpApp => ghostSignature(app)
    case app: PActualOpApp => actualSignature(app)
  }

  private def ghostArgsOfExp(op: PGhostOpApp): Seq[PExpression] = op match {
    case PCardinality(exp) => Seq(exp)
  }

  private def actualArgsOfExp(op: PActualOpApp): Seq[PExpression] = op match {
    case n: PDot => resolve(n).get match {
      case ap.AdtField(base, _, symb) => symb match {
        case SymbolTable.AdtDestructor(_, _, _) => Seq(base)
        case SymbolTable.AdtDiscriminator(_, _, _) => Seq(base)
        case SymbolTable.AdtClauseField(_, _, _, _) => ???
      }
      case _ => ???
    }
  }

  private[typing] def getArgsOfExp(op: POpApp): Seq[PExpression] = op match {
    case app: PGhostOpApp => ghostArgsOfExp(app)
    case app: PActualOpApp => actualArgsOfExp(app)
  }

}
