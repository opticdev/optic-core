package com.useoptic.diff.initial

import com.useoptic.diff.shapes.JsonTrail


case class ValueAffordanceSerialization( trail: JsonTrail,
                                         wasString: Boolean = false,
                                         wasNumber: Boolean = false,
                                         wasBoolean: Boolean = false,
                                         wasNull: Boolean = false,
                                         wasArray: Boolean = false,
                                         wasObject: Boolean = false,
                                         fieldSet: Set[Set[String]] )

case class ValueAffordanceSerializationWithCounter(affordances: Vector[ValueAffordanceSerialization], interactions: AffordanceInteractionPointers)
