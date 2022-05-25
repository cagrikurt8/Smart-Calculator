import scala.collection.mutable
import scala.io.StdIn.readLine

object main extends App {
  var condition = true
  var variableMap:mutable.Map[String, Int] = mutable.Map()
  var operatorStack:mutable.Stack[String] = mutable.Stack()
  var calculationStack:mutable.Stack[String] = mutable.Stack()

  while (condition) {
    var equation: Array[String] = Array()
    val entry = readLine()

    if (entry.contains("=")) {
      equation = entry.split("=")
      assignVariable(equation, variableMap)
    } else {
      if (entry.count(_ == '(') != entry.count(_ == ')')) {
        println(1)
        println("Invalid expression")
      } else {
        equation = entry.trim().split(" ")
        evaluate(equation, variableMap)
      }
    }
  }

  def assignVariable(equation:Array[String], variableMap:mutable.Map[String, Int]): Unit = {
    val invalidAlphaRegex = ".*?[a-zA-Z]+.*?"
    val validAlphaRegex = "[a-zA-Z]+"
    val variableName = equation(0).strip()
    val value = equation(1).strip()

    if (equation.length > 2) {
      println("Invalid assignment")
    } else if (!variableName.matches(validAlphaRegex)) {
      println("Invalid identifier")
    } else if (value.matches(invalidAlphaRegex)) {
      try {
        variableMap += variableName -> variableMap(value)
      } catch {
        case e: NoSuchElementException => println("Invalid assignment")
      }
    } else {
      variableMap += variableName -> value.toInt
    }
  }

  def evaluate(equation:Array[String], variableMap:mutable.Map[String, Int]): Unit = {
    val digitRegex = "\\-?\\d+"
    val alphaRegex = "[a-zA-Z]+"
    val plusRegex = "\\+\\d+"
    var unknown = false
    var result = ""

    if (equation.length > 1) {
      for (i <- equation.indices) {
        val num = equation(i)

        if (num.matches(digitRegex)) {
          result += num + " "
        } else if (variableMap.contains(num)) {
          result += variableMap(num).toString + " "
        } else if (num.matches(alphaRegex)) {
          println(2)
          println("Invalid expression")
          unknown = true
        } else if (num.contains("((")) {
          operatorStack.push("(")
          operatorStack.push("(")
          val temp = num.substring(2)

          if (temp.matches(digitRegex)) {
            result += temp + " "
          } else if (variableMap.contains(temp)) {
            result += variableMap(temp) + " "
          } else {
            println("Invalid expression")
            unknown = true
          }

        } else if (num.contains("(")) {
          operatorStack.push("(")
          val temp = num.substring(1)

          if (temp.matches(digitRegex)) {
            result += temp + " "
          } else if (variableMap.contains(temp)) {
            result += variableMap(temp) + " "
          } else {
            println("Invalid expression")
            unknown = true
          }

        } else if (num.contains(")")) {
          val temp = num.substring(0, num.indexOf(")"))

          if (temp.matches(digitRegex)) {
            result += temp + " "
            result = popAndPush(operatorStack, num, result)
          } else if (variableMap.contains(temp)) {
            result += variableMap(temp) + " "
            result = popAndPush(operatorStack, variableMap(temp).toString, result)
          } else {
            println(4)
            println("Invalid expression")
            unknown = true
          }

        } else if (operatorStack.isEmpty || operatorStack.top == "(") {
          if (num.contains("+") || num.contains("-") || num == "*" || num == "/") {
            operatorStack.push(num)
          } else {
            println("Invalid expression")
            unknown = true
          }
        }  else {
          val topOperator = operatorStack.top

          if (topOperator == "*" || topOperator == "/") {
            if (num == "/" || num == "*") {
              while (operatorStack.nonEmpty && (!operatorStack.top.contains("+") || !operatorStack.top.contains("-") || operatorStack.top != "(")) {
                result += operatorStack.pop() + " "
              }
              operatorStack.push(num)
            } else if (num.contains("+") || num.contains("-")) {
              result = popAndPush(operatorStack, num, result)
              operatorStack.push(num)
            } else {
              println("Invalid expression")
              unknown = true
            }
          } else if (topOperator.contains("+") || topOperator.contains("-")) {
            if (num == "/" || num == "*") {
              operatorStack.push(num)
            } else if (num.contains("+") || num.contains("-")) {
              result = popAndPush(operatorStack, num, result)
              operatorStack.push(num)
            } else {
              println("Invalid expression")
              unknown = true
            }
          }
        }

        if (i == equation.length - 1) {
          result = popAndPush(operatorStack, num, result)
        }
      }

      if (!unknown) {
        calculate(result)
      } else {
       unknown = false
      }

    } else if (equation(0).equals("/exit")){
      println("Bye!")
      condition = false
    } else if (equation(0).equals("/help")) {
      println("The program calculates the sum of equation")
    } else if (equation.length == 1 && equation(0).matches(digitRegex)) {
      println(equation(0))
    } else if (equation.length == 1 && equation(0).matches(plusRegex)) {
      println(equation(0).substring(1))
    } else if (equation(0).contains("/")) {
      println("Unknown command")
    } else if(equation(0).matches("")) {

    } else if (variableMap.contains(equation(0))){
      println(variableMap(equation(0)))
    } else {
      println("Unknown variable")
    }
  }

  def calculate(expression:String): Unit = {
    val digitRegex = "\\-?\\d+"
    val expressionArray = expression.split(" ")
    var unknown = false

    for (i <- expressionArray.indices) {
      val curr = expressionArray(i)

      if (curr.matches(digitRegex)) {
        calculationStack.push(curr)
      } else if (curr.contains("+")) {
        val num1 = calculationStack.pop().toInt
        val num2 = calculationStack.pop().toInt

        calculationStack.push((num1 + num2).toString)
      } else if (curr.contains("-")) {
        if (curr.length % 2 == 1) {
          val num1 = calculationStack.pop().toInt
          val num2 = calculationStack.pop().toInt

          calculationStack.push((num2 - num1).toString)
        } else {
          val num1 = calculationStack.pop().toInt
          val num2 = calculationStack.pop().toInt

          calculationStack.push((num1 + num2).toString)
        }
      } else if (curr == "*") {
        val num1 = calculationStack.pop().toInt
        val num2 = calculationStack.pop().toInt

        calculationStack.push((num1 * num2).toString)
      } else if (curr == "/") {
        val num1 = calculationStack.pop().toInt
        val num2 = calculationStack.pop().toInt

        calculationStack.push((num2 / num1).toString)
      } else {
        println("Invalid expression")
        unknown = true
      }
    }

    if (!unknown) {
      println(calculationStack.top)
    } else {
      unknown = false
    }
  }

  def popAndPush(operatorStack: mutable.Stack[String], num:String, result:String): String = {
    var temp = result
    while (operatorStack.nonEmpty) {
      if (operatorStack.top != "(") {
        temp += operatorStack.pop() + " "
      } else {
        operatorStack.pop()
        return temp
      }
    }
    return temp
  }
}
