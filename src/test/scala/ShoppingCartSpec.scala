import org.scalatest._

class ShoppingCartSpec extends FlatSpec with Matchers {
  "Apples" should "cost 60p" in {
    ShoppingCart.priceUp(Apple) should === (60)
  }

  "Oranges" should "cost 25p" in {
    ShoppingCart.priceUp(Orange) should === (25)
  }
  
  "checkout" should "takes a list of items scanned and outputs the total cost" in {
    ShoppingCart.checkOut(List(Apple, Apple, Orange, Apple)) should === (145)
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

  "shoppingCart process" should "take a list of Apples and Oranges, returning a price" in {
    ShoppingCart.process(Array("Apple", "Apple", "Orange", "Apple")) should === ("£1.45")
    ShoppingCart.process(Array("Apple", "Apple", "Orange", "Apple", "Orange", "Orange")) should === ("£1.70")
  }

  "checkout" should "offer buy one, get one free on Apples" in {
    ShoppingCart.checkOut(List(Apple)) should === (60)
    ShoppingCart.checkOut(List(Apple, Apple)) should === (60)
    ShoppingCart.checkOut(List(Apple, Apple, Apple)) should === (60 * 2)
    ShoppingCart.checkOut(List(Apple, Apple, Apple, Apple)) should === (60 * 2)
  }

  "checkout" should "offer 3 for the price of 2 on Oranges" in {
    ShoppingCart.checkOut(List(Orange)) should === (25)
    ShoppingCart.checkOut(List(Orange, Orange)) should === (25 * 2)
    ShoppingCart.checkOut(List(Orange, Orange, Orange)) should === (25 * 2)
    ShoppingCart.checkOut(List.fill(4)(Orange)) should === (25 * 3)
    ShoppingCart.checkOut(List.fill(5)(Orange)) should === (25 * 4)
    ShoppingCart.checkOut(List.fill(6)(Orange)) should === (25 * 4)
  }
}