package viper.gobra.ast.internal.utility

/** Code was taken from @see [[viper.silver.ast.utility.ViperStrategy]] */

import viper.gobra.ast.internal._
import viper.silver.ast.utility.Rewriter.Traverse.Traverse
import viper.silver.ast.utility.Rewriter.{Star => _, _}

/**
  * Gobra specific Wrapper for the rewriting Strategies
  * Provides automatic back transformations for Node rewrites
  *
  * @param p Partial function to perform rewritings
  * @tparam C Type of context
  */
class GobraStrategy[C <: Context[Node]](p: PartialFunction[(Node, C), Node]) extends Strategy[Node, C](p) {
  override def preserveMetaData(old: Node, now: Node, directlyRewritten: Boolean): Node = {
    GobraStrategy.preserveMetaData(old, now, directlyRewritten)
  }
}

/**
  * Gobra specific Wrapper for Regex Strategies
  * Provides automatic back transformations for Node to Node rewrites
  *
  * @param a The automaton generated from the regular expression
  * @param p PartialFunction that describes rewriting
  * @param d Default context
  * @tparam C Type of context
  */
class GobraRegexStrategy[C](a: TRegexAutomaton, p: PartialFunction[(Node, RegexContext[Node, C]), Node], d: PartialContextR[Node, C]) extends RegexStrategy[Node, C](a, p, d) {
  override def preserveMetaData(old: Node, now: Node, directlyRewritten: Boolean): Node = {
    GobraStrategy.preserveMetaData(old, now, directlyRewritten)
  }
}

class SlimGobraRegexStrategy[C](a: TRegexAutomaton, p: PartialFunction[Node, Node]) extends SlimRegexStrategy[Node](a, p) {
  override def preserveMetaData(old: Node, now: Node, directlyRewritten: Boolean): Node = {
    GobraStrategy.preserveMetaData(old, now, directlyRewritten)
  }
}


class GobraRegexBuilder[C](acc: (C, C) => C, comp: (C, C) => C, dflt: C) extends TreeRegexBuilder[Node, C](acc, comp, dflt) {

  /**
    * Generates a TreeRegexBuilderWithMatch by adding the matching part to the mix
    *
    * @param m Regular expression
    * @return TregexBuilderWithMatch that contains regex `f`
    */
  override def &>(m: Match): GobraRegexBuilderWithMatch[C] = new GobraRegexBuilderWithMatch[C](this, m)
}


class GobraRegexBuilderWithMatch[C](v: GobraRegexBuilder[C], m: Match) extends TreeRegexBuilderWithMatch[Node, C](v, m) {

  override def |->(p: PartialFunction[(Node, RegexContext[Node, C]), Node]): GobraRegexStrategy[C] = new GobraRegexStrategy[C](m.createAutomaton(), p, new PartialContextR[Node, C](v.default, v.accumulator, v.combinator))
}


class SlimGobraRegexBuilder {

  def &>(m: Match): SlimGobraRegexBuilderWithMatch = new SlimGobraRegexBuilderWithMatch(m)
}

class SlimGobraRegexBuilderWithMatch(regex: Match) {

  def |->(p: PartialFunction[Node, Node]): SlimGobraRegexStrategy[Node] = new SlimGobraRegexStrategy[Node](regex.createAutomaton(), p)
}

/**
  * Factory for standard rewriting configurations
  */
object GobraStrategy {

  def SlimRegex(m: Match, p: PartialFunction[Node, Node]): SlimGobraRegexStrategy[Node] = {
    new SlimGobraRegexBuilder &> m |-> p
  }

  def Regex[C](m: Match, p: PartialFunction[(Node, RegexContext[Node, C]), Node], default: C, acc: (C, C) => C, comb: (C, C) => C): GobraRegexStrategy[C] = {
    new GobraRegexBuilder[C](acc, comb, default) &> m |-> p
  }

  /**
    * Strategy without context
    *
    * @param p Partial function to perform rewriting
    * @param t Traversion mode
    * @return GobraStrategy
    */
  def Slim(p: PartialFunction[Node, Node], t: Traverse = Traverse.TopDown): Strategy[Node, SimpleContext[Node]] = {
    new GobraStrategy[SimpleContext[Node]](new AddArtificialContext(p)) defaultContext new NoContext[Node] traverse t
  }

  /**
    * Strategy with context about ancestors and siblings
    *
    * @param p Partial function to perform rewriting
    * @param t Traversion mode
    * @return GobraStrategy
    */
  def Ancestor(p: PartialFunction[(Node, ContextA[Node]), Node], t: Traverse = Traverse.TopDown): Strategy[Node, ContextA[Node]] = {
    new GobraStrategy[ContextA[Node]](p) defaultContext new PartialContextA[Node] traverse t
  }

  /**
    * Strategy with context about ancestors, siblings and custom context
    *
    * @param p          Partial function to perform rewriting
    * @param default    Default context
    * @param updateFunc Function that specifies how to update the custom context
    * @param t          Traversion mode
    * @tparam C Type of custom context
    * @return GobraStrategy
    */
  def Context[C](p: PartialFunction[(Node, ContextC[Node, C]), Node], default: C, updateFunc: PartialFunction[(Node, C), C] = PartialFunction.empty, t: Traverse = Traverse.TopDown): Strategy[Node, ContextC[Node, C]] = {
    new GobraStrategy[ContextC[Node, C]](p) defaultContext new PartialContextC[Node, C](default, updateFunc) traverse t
  }

  // AS: It might be more efficient to implement this one natively, and make Context a special case of it, rather than building a richer partial function and then desugaring whenever needed
  /**
    * Strategy with (only) custom context
    *
    * @param p          Partial function to perform rewriting
    * @param initialContext    Default context
    * @param updateFunc Function that specifies how to update the custom context
    * @param t          Traversion mode
    * @tparam C Type of custom context
    * @return GobraStrategy
    */
  def SimpleContext[C](p: PartialFunction[(Node, C), Node], initialContext: C, updateFunc: PartialFunction[(Node, C), C] = PartialFunction.empty, t: Traverse = Traverse.TopDown): Strategy[Node, ContextC[Node, C]] = {
    Context[C]({ // rewrite partial function taking context with parent access etc. to one just taking the custom context
      case (n, generalContext) if p.isDefinedAt(n, generalContext.c) => p.apply(n, generalContext.c)
    },
      initialContext, updateFunc, t
    )
  }

  /**
    * Function for automatic Error back transformation of nodes and conservation of metadata
    */
  def preserveMetaData(old: Node, now: Node, directlyRewritten: Boolean): Node = {
    /** @see [[viper.silver.ast.utility.ViperStrategy]] if meta data changes */
    now
  }


  def gobraDuplicatorStrategy: PartialFunction[(Node, Seq[AnyRef], Node.Meta), Node] = {
    case (n, args, info) => gobraDuplicator(n, args, info)
  }

  def gobraDuplicator[N <: Node](x: N, args: Seq[AnyRef], meta: Node.Meta): N = {
    val node: Node = (x, args) match {
        // Members
      case (p: Program, Seq(t: Vector[TopType@unchecked], v: Vector[GlobalVarDecl@unchecked], c: Vector[GlobalConst@unchecked], m: Vector[Method@unchecked], f: Vector[Function@unchecked])) => Program(t,v,c,m,f)(meta)
      case (m: Method, Seq(rec: Parameter, arg: Vector[Parameter@unchecked], res: Vector[LocalVar@unchecked], pre: Vector[Assertion@unchecked], post: Vector[Assertion@unchecked], b: Option[Block@unchecked])) => Method(rec, m.name, arg, res, pre, post, b)(meta)
      case (f: Function, Seq(arg: Vector[Parameter@unchecked], res: Vector[LocalVar@unchecked], pre: Vector[Assertion@unchecked], post: Vector[Assertion@unchecked], b: Option[Block@unchecked])) => Function(f.name, arg, res, pre, post, b)(meta)
        // Statements
      case (b: Block, Seq(v: Vector[LocalVar@unchecked], s: Vector[Stmt@unchecked])) => Block(v, s)(meta)
      case (s: Seqn, Seq(stmts: Vector[Stmt@unchecked])) => Seqn(stmts)(meta)
      case (s: SingleAss, Seq(l: Assignee, r: Expr)) => SingleAss(l, r)(meta)
      case (m: MultiAss, Seq(l: Vector[Assignee@unchecked], r: Expr)) => MultiAss(l, r)(meta)
      case (a: Assignee.Var, Seq(v: BodyVar)) => Assignee.Var(v)
      case (a: Assignee.Pointer, Seq(e: Deref)) => Assignee.Pointer(e)
      case (r: Return, Seq()) => Return()(meta)
      case (a: Assert, Seq(ass: Assertion)) => Assert(ass)(meta)
      case (a: Assume, Seq(ass: Assertion)) => Assume(ass)(meta)
      case (i: Inhale, Seq(ass: Assertion)) => Inhale(ass)(meta)
      case (e: Exhale, Seq(ass: Assertion)) => Exhale(ass)(meta)
        // Assertions
      case (s: Star, Seq(l: Assertion, r: Assertion)) => Star(l, r)(meta)
      case (e: ExprAssertion, Seq(exp: Expr)) => ExprAssertion(exp)(meta)
      case (i: Implication, Seq(l: Expr, r: Assertion)) => Implication(l, r)(meta)
      case (a: Access, Seq(acc: Accessible)) => Access(acc)(meta)
      case (a: Accessible.Ref, Seq(d: Deref)) => Accessible.Ref(d)
        // Expressions
      case (d: DfltVal, Seq()) => DfltVal(d.typ)(meta)
      case (d: Deref, Seq(e: Expr)) => Deref(e, d.typ)(meta)
      case (r: Ref, Seq(ref: Addressable, t: PointerT)) => Ref(ref, t)(meta)
      case (i: IntLit, Seq()) => IntLit(i.v)(meta)
      case (b: BoolLit, Seq()) => BoolLit(b.b)(meta)
      case (p: Parameter, Seq()) => Parameter(p.id, p.typ)(meta)
      case (l: LocalVar.Val, Seq()) => LocalVar.Val(l.id, l.typ)(meta)
      case (l: LocalVar.Ref, Seq()) => LocalVar.Ref(l.id, l.typ)(meta)
      case (a: Addressable.Var, Seq(v: LocalVar.Ref)) => Addressable.Var(v)

      case _ => ???
    }

    node.asInstanceOf[N]
  }
}