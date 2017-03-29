package com.github.zenpie.macrowave.support

import com.github.zenpie.macrowave._

trait SingletonRule[T] {
  type Out
}

object SingletonRule extends SingletonRuleLowPriorityImplicits {

  type Aux[T, Out0] = SingletonRule[T] {
    type Out = Out0
  }

  implicit val hnil: SingletonRule.Aux[HNil, HNil] = new SingletonRule[HNil] {
    type Out = HNil
  }

  implicit def hcons1[T]: SingletonRule.Aux[T :: HNil, T] = new SingletonRule[T :: HNil] {
    type Out = T
  }

}

trait SingletonRuleLowPriorityImplicits {

  implicit def hconsN[H, T <: HList]: SingletonRule.Aux[H :: T, H :: T] = new SingletonRule[H :: T] {
    type Out = H :: T
  }

}
