// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2020 ETH Zurich.

package viper.gobra.ast.internal.utility

/** Code was taken from @see [[viper.silver.ast.utility.ViperStrategy]] */

import viper.gobra.ast.internal._

/**
  * Factory for standard rewriting configurations
  */
object GobraStrategy {

  def gobraDuplicatorStrategy: PartialFunction[(Node, Seq[AnyRef], Node.Meta), Node] = {
    case (n, args, info) => gobraDuplicator(n, args, info)
  }

  def gobraDuplicator[N <: Node](x: N, args: Seq[AnyRef], meta: Node.Meta): N = {
    val node: Node = (x, args) match {
        // Members
      case (p: Program, Seq(t: Vector[TopType@unchecked], m: Vector[Member@unchecked])) => Program(t, m, p.table)(meta)
      case (_: Method, Seq(rec: Parameter.In, name: MethodProxy, arg: Vector[Parameter.In@unchecked], res: Vector[Parameter.Out@unchecked], pre: Vector[Assertion@unchecked], post: Vector[Assertion@unchecked], b: Option[Block@unchecked])) => Method(rec, name, arg, res, pre, post, b)(meta)
      case (_: PureMethod, Seq(rec: Parameter.In, name: MethodProxy, arg: Vector[Parameter.In@unchecked], res: Vector[Parameter.Out@unchecked], pre: Vector[Assertion@unchecked], post: Vector[Assertion@unchecked], b: Option[Expr@unchecked])) => PureMethod(rec, name, arg, res, pre, post, b)(meta)
      case (_: Function, Seq(name: FunctionProxy, arg: Vector[Parameter.In@unchecked], res: Vector[Parameter.Out@unchecked], pre: Vector[Assertion@unchecked], post: Vector[Assertion@unchecked], b: Option[Block@unchecked])) => Function(name, arg, res, pre, post, b)(meta)
      case (_: PureFunction, Seq(name: FunctionProxy, arg: Vector[Parameter.In@unchecked], res: Vector[Parameter.Out@unchecked], pre: Vector[Assertion@unchecked], post: Vector[Assertion@unchecked], b: Option[Expr@unchecked])) => PureFunction(name, arg, res, pre, post, b)(meta)
      case (_: MPredicate, Seq(recv: Parameter.In, name: MPredicateProxy, args: Vector[Parameter.In@unchecked], b: Option[Assertion@unchecked])) => MPredicate(recv, name, args, b)(meta)
      case (_: FPredicate, Seq(name: FPredicateProxy, args: Vector[Parameter.In@unchecked], b: Option[Assertion@unchecked])) => FPredicate(name, args, b)(meta)
      case (f: Field, Seq()) => Field(f.name, f.typ, f.ghost)(meta)
        // Statements
      case (_: Block, Seq(v: Vector[BlockDeclaration@unchecked], s: Vector[Stmt@unchecked])) => Block(v, s)(meta)
      case (_: Seqn, Seq(stmts: Vector[Stmt@unchecked])) => Seqn(stmts)(meta)
      case (_: If, Seq(cond: Expr, thn: Stmt, els: Stmt)) => If(cond, thn, els)(meta)
      case (_: While, Seq(cond: Expr, invs: Vector[Assertion@unchecked], body: Stmt)) => While(cond, invs, body)(meta)
      case (_: Make, Seq(target: LocalVar, co: CompositeObject)) => Make(target, co)(meta)
      case (_: SingleAss, Seq(l: Assignee, r: Expr)) => SingleAss(l, r)(meta)
      case (_: Assignee.Var, Seq(v: AssignableVar)) => Assignee.Var(v)
      case (_: Assignee.Pointer, Seq(e: Deref)) => Assignee.Pointer(e)
      case (_: Assignee.Field, Seq(e: FieldRef)) => Assignee.Field(e)
      case (_: Assignee.Index, Seq(e: IndexedExp)) => Assignee.Index(e)
      case (_: FunctionCall, Seq(targets: Vector[LocalVar@unchecked], func: FunctionProxy, args: Vector[Expr@unchecked])) => FunctionCall(targets, func, args)(meta)
      case (_: MethodCall, Seq(targets: Vector[LocalVar@unchecked], recv: Expr, meth: MethodProxy, args: Vector[Expr@unchecked])) => MethodCall(targets, recv, meth, args)(meta)
      case (_: Return, Seq()) => Return()(meta)
      case (_: Assert, Seq(ass: Assertion)) => Assert(ass)(meta)
      case (_: Assume, Seq(ass: Assertion)) => Assume(ass)(meta)
      case (_: Inhale, Seq(ass: Assertion)) => Inhale(ass)(meta)
      case (_: Exhale, Seq(ass: Assertion)) => Exhale(ass)(meta)
      case (_: Fold, Seq(acc: Access)) => Fold(acc)(meta)
      case (_: Unfold, Seq(acc: Access)) => Unfold(acc)(meta)
      case (s: SafeTypeAssertion, Seq(resTarget: LocalVar, successTarget: LocalVar, expr: Expr)) => SafeTypeAssertion(resTarget, successTarget, expr, s.typ)(meta)
        // Assertions
      case (_: SepAnd, Seq(l: Assertion, r: Assertion)) => SepAnd(l, r)(meta)
      case (_: ExprAssertion, Seq(exp: Expr)) => ExprAssertion(exp)(meta)
      case (_: Implication, Seq(l: Expr, r: Assertion)) => Implication(l, r)(meta)
      case (_: Access, Seq(acc: Accessible, perm: Permission)) => Access(acc, perm)(meta)
      case (_: Accessible.Address, Seq(d: Deref)) => Accessible.Address(d)
      case (_: Accessible.Predicate, Seq(p: PredicateAccess)) => Accessible.Predicate(p)
      case (_: FPredicateAccess, Seq(pred: FPredicateProxy, args: Vector[Expr@unchecked])) => FPredicateAccess(pred, args)(meta)
      case (_: MPredicateAccess, Seq(recv: Expr, pred: MPredicateProxy, args: Vector[Expr@unchecked])) => MPredicateAccess(recv, pred, args)(meta)
      case (_: MemoryPredicateAccess, Seq(arg: Expr)) => MemoryPredicateAccess(arg)(meta)
        // Expressions
      case (_: Unfolding, Seq(acc: Access, e: Expr)) => Unfolding(acc, e)(meta)
      case (f: PureFunctionCall, Seq(func: FunctionProxy, args: Vector[Expr@unchecked])) => PureFunctionCall(func, args, f.typ)(meta)
      case (m: PureMethodCall, Seq(recv: Expr, meth: MethodProxy, args: Vector[Expr@unchecked])) => PureMethodCall(recv, meth, args, m.typ)(meta)
      case (d: DfltVal, Seq()) => DfltVal(d.typ)(meta)
      case (_: Tuple, Seq(args: Vector[Expr@unchecked])) => Tuple(args)(meta)
      case (d: Deref, Seq(e: Expr)) => Deref(e, d.typ)(meta)
      case (_: Ref, Seq(ref: Addressable, t: PointerT)) => Ref(ref, t)(meta)
      case (_: FieldRef, Seq(recv: Expr, field: Field)) => FieldRef(recv, field)(meta)
      case (_: IndexedExp, Seq(base: Expr, idx: Expr)) => IndexedExp(base, idx)(meta)
      case (_: ArrayUpdate, Seq(base: Expr, left: Expr, right: Expr)) => ArrayUpdate(base, left, right)(meta)
      case (_: StructUpdate, Seq(base: Expr, left: Field, right: Expr)) => StructUpdate(base, left, right)(meta)
      case (_: Negation, Seq(op: Expr)) => Negation(op)(meta)
      case (_: EqCmp, Seq(l: Expr, r: Expr)) => EqCmp(l, r)(meta)
      case (_: UneqCmp, Seq(l: Expr, r: Expr)) => UneqCmp(l, r)(meta)
      case (_: LessCmp, Seq(l: Expr, r: Expr)) => LessCmp(l, r)(meta)
      case (_: AtMostCmp, Seq(l: Expr, r: Expr)) => AtMostCmp(l, r)(meta)
      case (_: GreaterCmp, Seq(l: Expr, r: Expr)) => GreaterCmp(l, r)(meta)
      case (_: AtLeastCmp, Seq(l: Expr, r: Expr)) => AtLeastCmp(l, r)(meta)
      case (_: And, Seq(l: Expr, r: Expr)) => And(l, r)(meta)
      case (_: Or, Seq(l: Expr, r: Expr)) => Or(l, r)(meta)
      case (_: And, Seq(l: Expr, r: Expr)) => And(l, r)(meta)
      case (_: Sub, Seq(l: Expr, r: Expr)) => Sub(l, r)(meta)
      case (_: Mul, Seq(l: Expr, r: Expr)) => Mul(l, r)(meta)
      case (_: Mod, Seq(l: Expr, r: Expr)) => Mod(l, r)(meta)
      case (_: Div, Seq(l: Expr, r: Expr)) => Div(l, r)(meta)
      case (e: TypeAssertion, Seq(exp: Expr)) => TypeAssertion(exp, e.arg)(meta)
      case (_: TypeOf, Seq(exp: Expr)) => TypeOf(exp)(meta)
      case (_: IsComparableInterface, Seq(exp: Expr)) => IsComparableInterface(exp)(meta)
      case (_: IsComparableType, Seq(exp: Expr)) => IsComparableType(exp)(meta)
      case (e: ToInterface, Seq(exp: Expr)) => ToInterface(exp, e.typ)(meta)
      case (_: BoolTExpr, Seq()) => BoolTExpr()(meta)
      case (e: IntTExpr, Seq()) => IntTExpr(e.kind)(meta)
      case (_: PermTExpr, Seq()) => PermTExpr()(meta)
      case (e: DefinedTExpr, Seq()) => DefinedTExpr(e.name)(meta)
      case (_: PointerTExpr, Seq(elems: Expr)) => PointerTExpr(elems)(meta)
      case (e: StructTExpr, Seq()) => StructTExpr(e.fields)(meta)
      case (_: ArrayTExpr, Seq(length: Expr, elems: Expr)) => ArrayTExpr(length, elems)(meta)
      case (_: SliceTExpr, Seq(elems: Expr)) => SliceTExpr(elems)(meta)
      case (_: SequenceTExpr, Seq(elems: Expr)) => SequenceTExpr(elems)(meta)
      case (_: SetTExpr, Seq(elems: Expr)) => SetTExpr(elems)(meta)
      case (_: MultisetTExpr, Seq(elems: Expr)) => MultisetTExpr(elems)(meta)
      case (_: OptionTExpr, Seq(elems: Expr)) => OptionTExpr(elems)(meta)
      case (_: TupleTExpr, Seq(elems: Vector[Expr@unchecked])) => TupleTExpr(elems)(meta)
      case (_: Multiplicity, Seq(left: Expr, right: Expr)) => Multiplicity(left, right)(meta)
      case (_: Length, Seq(exp: Expr)) => Length(exp)(meta)
      case (_: Capacity, Seq(exp: Expr)) => Capacity(exp)(meta)
      case (_: RangeSequence, Seq(low: Expr, high: Expr)) => RangeSequence(low, high)(meta)
      case (_: SequenceAppend, Seq(l: Expr, r: Expr)) => SequenceAppend(l, r)(meta)
      case (_: SequenceUpdate, Seq(base: Expr, left: Expr, right: Expr)) => SequenceUpdate(base, left, right)(meta)
      case (_: SequenceDrop, Seq(l: Expr, r: Expr)) => SequenceDrop(l, r)(meta)
      case (_: SequenceTake, Seq(l: Expr, r: Expr)) => SequenceTake(l, r)(meta)
      case (_: SequenceConversion, Seq(arg: Expr)) => SequenceConversion(arg)(meta)
      case (_: Union, Seq(l: Expr, r: Expr)) => Union(l, r)(meta)
      case (_: Intersection, Seq(l: Expr, r: Expr)) => Intersection(l, r)(meta)
      case (_: SetMinus, Seq(l: Expr, r: Expr)) => SetMinus(l, r)(meta)
      case (_: Subset, Seq(l: Expr, r: Expr)) => Subset(l ,r)(meta)
      case (_: Cardinality, Seq(arg: Expr)) => Cardinality(arg)(meta)
      case (_: Contains, Seq(l: Expr, r: Expr)) => Contains(l, r)(meta)
      case (_: SetConversion, Seq(arg: Expr)) => SetConversion(arg)(meta)
      case (_: MultisetConversion, Seq(arg: Expr)) => MultisetConversion(arg)(meta)
      case (_: OptionNone, Seq(t : Type)) => OptionNone(t)(meta)
      case (_: OptionSome, Seq(op : Expr)) => OptionSome(op)(meta)
      case (_: OptionGet, Seq(op : Expr)) => OptionGet(op)(meta)
      case (_: Slice, Seq(base : Expr, low : Expr, high : Expr, max : Option[Expr@unchecked])) => Slice(base, low, high, max)(meta)
      case (e: Old, Seq(op: Expr)) => Old(op, e.typ)(meta)
      case (c: Conditional, Seq(cond: Expr, thn: Expr, els: Expr)) => Conditional(cond, thn, els, c.typ)(meta)
      case (_: Trigger, Seq(exprs: Vector[Expr@unchecked])) => Trigger(exprs)(meta)
      case (_: PureForall, Seq(vars: Vector[BoundVar@unchecked], triggers: Vector[Trigger@unchecked], body: Expr)) => PureForall(vars, triggers, body)(meta)
      case (_: SepForall, Seq(vars: Vector[BoundVar@unchecked], triggers: Vector[Trigger@unchecked], body: Assertion)) => SepForall(vars, triggers, body)(meta)
      case (_: Exists, Seq(vars: Vector[BoundVar@unchecked], triggers: Vector[Trigger@unchecked], body: Expr)) => Exists(vars, triggers, body)(meta)
      case (_: FullPerm, Seq()) => FullPerm(meta)
      case (_: NoPerm, Seq()) => NoPerm(meta)
      case (_: FractionalPerm, Seq(left: Expr, right: Expr)) => FractionalPerm(left, right)(meta)
      case (_: WildcardPerm, Seq()) => WildcardPerm(meta)
      case (i: IntLit, Seq()) => IntLit(i.v)(meta)
      case (b: BoolLit, Seq()) => BoolLit(b.b)(meta)
      case (n: NilLit, Seq()) => NilLit(n.typ)(meta)
      case (s: StructLit, Seq(args: Vector[Expr@unchecked])) => StructLit(s.typ, args)(meta)
      case (_: ArrayLit, Seq(t: Type, args: Vector[Expr@unchecked])) => ArrayLit(t, args)(meta)
      case (_: SliceLit, Seq(t: Type, args: Vector[Expr@unchecked])) => SliceLit(t, args)(meta)
      case (_: SequenceLit, Seq(t: Type, args: Vector[Expr@unchecked])) => SequenceLit(t, args)(meta)
      case (_: SetLit, Seq(t: Type, args: Vector[Expr@unchecked])) => SetLit(t, args)(meta)
      case (_: MultisetLit, Seq(t: Type, args: Vector[Expr@unchecked])) => MultisetLit(t, args)(meta)
      case (p: Parameter.In, Seq()) => Parameter.In(p.id, p.typ)(meta)
      case (p: Parameter.Out, Seq()) => Parameter.Out(p.id, p.typ)(meta)
      case (l: LocalVar, Seq()) => LocalVar(l.id, l.typ)(meta)
      case (_: Addressable.Var, Seq(v: LocalVar)) => Addressable.Var(v)
      case (_: Addressable.Pointer, Seq(v: Deref)) => Addressable.Pointer(v)
      case (_: Addressable.Field, Seq(v: FieldRef)) => Addressable.Field(v)
      case (_: Addressable.Index, Seq(v: IndexedExp)) => Addressable.Index(v)
        // Proxy
      case (f: FunctionProxy, Seq()) => FunctionProxy(f.name)(meta)
      case (m: MethodProxy, Seq()) => MethodProxy(m.name, m.uniqueName)(meta)
      case (f: FPredicateProxy, Seq()) => FPredicateProxy(f.name)(meta)
      case (m: MPredicateProxy, Seq()) => MPredicateProxy(m.name, m.uniqueName)(meta)
    }

    node.asInstanceOf[N]
  }
}
