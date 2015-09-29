import org.scalatest._

class ShoppingCartSpec extends FlatSpec with Matchers {
  "Apples" should "cost 60p" in {
    ShoppingCart.priceUp(Apple) should === (60)
  }

  "Oranges" should "cost 25p" in {
    ShoppingCart.priceUp(Orange) should === (25)
  }
  
  "checkout" should "takes a list of items scanned and outputs the total cost" in {
    ShoppingCart.checkOut(List(Apple, Apple, Orange, Apple)) should === (205)
  }
  "checkout" should "have a zero price for an empty list" in {
    ShoppingCart.checkOut(Nil) should === (0)
  }

  "scanItems" should "take a list of strings and return list of items as the basket" in {
    ShoppingCart.scanItems(List("Apple", "Orange")) should === (List(Apple, Orange))
  }

  "scanItems" should "reject items which are unknown" in {
    an [IllegalArgumentException] should be thrownBy
      ShoppingCart.scanItems(List("Apple", "Banana", "Orange"))
  }

  "shoppingCart" should "take a list of Apples and Oranges" in {
    ShoppingCart.main(Array("Apple","Orange"))
  }
}