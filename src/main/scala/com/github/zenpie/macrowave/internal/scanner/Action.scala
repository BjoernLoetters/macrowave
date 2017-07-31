package com.github.zenpie.macrowave.internal.scanner

import com.github.zenpie.macrowave.internal.ids.TerminalId

sealed trait Action
case object NoAction extends Action
case class TokenAction(id: TerminalId) extends Action
