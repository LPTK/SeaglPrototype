package front2

sealed trait Modifier
  
case object Type extends Modifier

case object Value extends Modifier

case object Rec extends Modifier

case object Priv extends Modifier


