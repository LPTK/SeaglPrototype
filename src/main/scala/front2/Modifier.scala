package front2

import utils._

// AST modifiers

sealed trait Modifier
  
// TODO: put in object:

case object Type extends Modifier

case object Value extends Modifier

case object Rec extends Modifier

case object Priv extends Modifier

// Summary class (some modifiers provide info that is stored elsewhere after ANF transformation)

case class Modification(priv: Bool)

