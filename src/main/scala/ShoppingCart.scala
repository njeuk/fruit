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

  def checkOut(basket: List[Item]) : Int = {

    def applyOffers : List[Item] = {
      def inner(apples: Int, oranges: Int, items: List[Item]) : List[Item] =
        items match {
          case Nil => List()
          case Apple::t =>
            if (apples == 1) inner(0, oranges, t)
            else Apple::inner(apples + 1, oranges, t)
          case Orange::t =>
            if (oranges == 2) inner(apples, 0, t)
            else Orange::inner(apples, oranges + 1, t)
      }

      inner(0, 0, basket)
    }

    val pricingBasket = applyOffers
    pricingBasket.foldLeft(0)(_ + priceUp(_))
  }

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
