sealed trait Item

case object Apple extends Item
case object Orange extends Item

object ShoppingCart {

  def priceUp(item: Item) = item match {
    case Apple => 60
    case Orange => 25
  }

  def checkOut(basket: List[Item]) = basket.foldLeft(0)(_ + priceUp(_))

  def scanItems(items: List[String]) : List[Item] = {
    items match {
      case Nil => List()
      case h::t => h match {
        case "Apple" => Apple :: scanItems(t)
        case "Orange" => Orange :: scanItems(t)
        case _ => throw new IllegalArgumentException
      }
    }
  }

  def main(args: Array[String]) {
    println(s"${checkOut(scanItems(args.toList))}p")
  }

}
