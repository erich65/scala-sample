/**
 * Copyright 2013 Eric Rich
 *
 * A singleton test class for some basic old school collections that 
 * aren't particularly useful as scala has similar built in stuctures
 * that are also not widely used as scala being a lisp/scheme like language
 * handles these tructures very well with simple syntax
 *
 * usage java CollectionTest command [value1, value2, value3...]
 * commnands:
 *   stack
 *   queue if a token is equal to "queue" then the test will enqueue the token then call dequeue for testing the stack flip
 */
object CollectionTest {


	/**
	 * Allows a collection to be tested
	 */	
	trait Testable{
		def test(values: List[String]);
	}
	
	/**
	 * A simple stack implemented with a list
	 */	
	class EStack extends Testable
	{
		//The list that manages the stack
		var stack: List[String] = List()

		/**
		 * Print the list of values
		 * Push all the values onto the stack and print the list
		 * Print each item as it's popped off the stack
		 */
		def test(values: List[String]){
			
			println("values: " + values)
			for(value <- values)
			{
				push(value);
			}
			println("filled stack: " + stack)
			print("popped values: " )

			while(!isEmpty)
			{
			   print(pop + ",")
			}
			println;
		}
		
		/**
		 * Last in first out
		 */
		def pop(): String = {
			var returnValue: String = null
			
			if(!stack.isEmpty)
			{
				returnValue = stack.head
				stack = stack.tail
			}

			returnValue
		}
		/**
		 * add an item to the stack
		 */
		def push(value: String){
			stack = value :: stack
		}
		/**
		 * Is Empty
		 */
		def isEmpty: Boolean = stack.isEmpty
		
		override def toString = stack.toString
		
	}
	/**
	 * Oldie but a goodie.  Use 2 stacks to implement a queue
	 */
	class EQueue extends Testable{

		// 2 stacks to manage the queue
		var stackIn: EStack = new EStack
		var stackOut: EStack = new EStack

		/**
		 * print the values
		 * enqueue all the values (deQueue if triggered by a deQueue token)
		 * print the stackIn and StackOut
		 * print all the values as they are dequeued
		 */
		def test(values: List[String]){
			
			println("values: " + values)
			for(value <- values)
			{
				enQueue(value);
				if("dequeue" == value)
					deQueue
			}
			println("stackIn: " + stackIn)
			println("stackOut: " + stackOut)
			print("dequeued values: " )

			while(!isEmpty)
			{
			   print(deQueue + ",")
			}
			println;
		}
		
		/**
		 * first in first out
		 */
		def deQueue(): String = {
			var returnValue: String = null
			
			if(!stackOut.isEmpty)
				returnValue = stackOut.pop
			else
			{
				while(!stackIn.isEmpty)
					stackOut.push(stackIn.pop);
	
				if(!stackOut.isEmpty)
					returnValue = stackOut.pop
			}

			returnValue
		}
		//Add an item to the queue
		def enQueue(value: String){
			stackIn.push(value)
		}
		//isempty
		def isEmpty : Boolean = stackIn.isEmpty & stackOut.isEmpty
	}

	/**
	 * A non singleton class that handles the testing logic
	 */
	class Collection {

		def command(command: String,values: List[String]){
			var tester: Testable = null

			if( command == "stack")
				tester = new EStack
			else if( command == "queue")
				tester = new EQueue

			if(tester != null)
				tester.test(values)
		}

	}
	
	//Entry point
	def main(args: Array[String]) {
		
		var list: List[String] = args.toList
		if(list.isEmpty){
			println("Usage: command [value1, value2, value3 ...]")
		}
		else{
			var command: String = args.toList.head
			val collection = new Collection
			collection.command(command,args.tail.toList)
		}
		
	}
}
