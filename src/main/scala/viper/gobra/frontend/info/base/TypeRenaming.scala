package viper.gobra.frontend.info.base

import viper.gobra.frontend.info.base.Type.{TypeVar}

class TypeRenaming(val mm: Map[TypeVar,TypeVar])
  extends TypeSubstitution(mm.map(kv => kv._1 -> kv._2))
{
  def +(kv: (TypeVar, TypeVar)): TypeRenaming = new TypeRenaming(mm + (kv._1->kv._2))
  def getS(key: TypeVar) : Option[TypeVar] = mm.get(key)

  def rename(key:TypeVar) : TypeVar = getS(key) match{ case Some(s) => s case None => key }
}
