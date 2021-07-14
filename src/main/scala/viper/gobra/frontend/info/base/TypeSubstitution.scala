package viper.gobra.frontend.info.base

import viper.gobra.frontend.info.base.Type._

import scala.language.implicitConversions

class TypeSubstitution(val m: Map[TypeVar, Type])
{

  //Reimplemented Map functions
  def get(key: TypeVar): Option[Type] = m.get(key)

  def getOrElse(key: TypeVar, alternative: Type): Type = m.getOrElse(key, alternative)

  def contains(value: TypeVar): Boolean = get(value).nonEmpty

  def + (entry: (TypeVar, Type)): TypeSubstitution = TypeSubstitution(m + entry)

  def keySet: Set[TypeVar] = m.keySet

  def map[B](f: ((TypeVar, Type)) => B): Seq[B] = m.map(f).toSeq

  def mapTypes(f: Type => Type): TypeSubstitution = {
    TypeSubstitution(m.map(keyValue => keyValue._1 -> f(keyValue._2)))
  }

  def compose(other: TypeSubstitution): TypeSubstitution = {
    mapTypes(_.substitute(other))
  }

  def ++ (substitution: TypeSubstitution): TypeSubstitution = {
    TypeSubstitution(
      this.m ++ substitution.m
    )
  }

  //Class methods

  def substitute(s: TypeVar, t: Type) : TypeSubstitution = {
    require(!contains(s))
    val ts = TypeSubstitution(Map(s -> t))
    TypeSubstitution(m.map({
      case (key, value) => key -> value.substitute(ts)
    }))
  }

  def add(a: String, b: Type): Option[TypeSubstitution] = add(FreeTypeVar(a), b)

  def add(a: Type, b: Type): Option[TypeSubstitution] = {
    val substitutedA = a.substitute(this)
    val substitutedB = b.substitute(this)
    println(s"Adding $substitutedA = $substitutedB")
    (substitutedA, substitutedB) match {
      case (_, _) if substitutedA == substitutedB => Some(this)
      case (ftv: TypeVar, t) => Some(substitute(ftv, t) + (ftv -> t))
      case (_, _: TypeVar) => add(substitutedB, substitutedA)
      case (g1: GenericType, g2: GenericType) if g1.genericName == g2.genericName => {
        println(s"Adding generic Types $g1 on $g2")
        (g1.typeArguments zip g2.typeArguments).foldLeft[Option[TypeSubstitution]](Some(this))(
          (res: Option[TypeSubstitution], p: (Type, Type)) => res match {
            case Some(ts) =>
              ts.add(p._1, p._2)
            case None => None
          }
        )
      }
      case _ => None
    }
  }


  def this(s: Seq[(TypeVar, Type)]) = this(s.toMap)

  override def toString: String = s"TypeSubstitution($m)"
}

object TypeSubstitution {
  val id = new TypeSubstitution(Map.empty[TypeVar, Type])
  implicit def apply(m: Map[TypeVar, Type]): TypeSubstitution = new TypeSubstitution(m)
  val defaultType: Int.type = Int

  def unifySequenceWithSubstitutions(
                                                      signatures: Seq[TypeSubstitution],
                                                      argData: Seq[(Type, Type, Seq[TypeSubstitution])]
                                                    ): Seq[TypeSubstitution] = {

    def unifyArgumentType(sigs: Seq[TypeSubstitution],
                          triple: (Type, Type, Seq[TypeSubstitution])): Seq[TypeSubstitution] = {
      val (t1, t2, substitutions) = triple
      println(s"$t1 = $t2 ; $substitutions")
      val signaturesWithSubs: Seq[Option[TypeSubstitution]] = for {
        sig <- sigs
        argumentSubstitutions <- substitutions
      } yield composeAndAdd(sig, argumentSubstitutions, t1, t2)
      println(s"Signatures with applied substitutions: $signaturesWithSubs")
      signaturesWithSubs.flatten
    }

    println(s"unify argData: $argData")

    argData.foldLeft(signatures)({
      case (sig, triple) => unifyArgumentType(sig, triple)
    })

  }

  def composeAndAdd(ts1: TypeSubstitution,
                                    ts2: TypeSubstitution,
                                    t1: Type,
                                    t2: Type): Option[TypeSubstitution] = {
    val sharedTypeVars = ts1.keySet.intersect(ts2.keySet)
    if (sharedTypeVars.exists(v => ts1.get(v).get != ts2.get(v).get)) {
      None
    } else {
      println(s"ts1 composed with ts2: ${ts1.compose(ts2)}")
      println(s"ts2 composed with ts1: ${ts2.compose(ts1)}")
      val res = (ts1.compose(ts2) ++ ts2.compose(ts1)).add(t1, t2)
      println(res)
      res
    }
  }

  def refreshWith(substitution: TypeSubstitution, renaming: TypeRenaming): TypeSubstitution = {
    new TypeSubstitution((substitution map {case (k, v) => renaming.rename(k) -> v.substitute(renaming)}).toMap)
  }
}
