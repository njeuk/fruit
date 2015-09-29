sealed trait Item

case object Apple extends Item
case object Orange extends Item

object ShoppingCart {

  def main(args: Array[String]) = println(process(args))

  def process(items: Array[String]) = f"£${checkOut(scanItems(items.toList)) / 100.0}%.2f"

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




}
