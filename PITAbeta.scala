
//DATABASE STUFF


	import java.sql.{Connection,DriverManager}
	Class.forName("com.mysql.jdbc.Driver")
	var connection = DriverManager.getConnection("jdbc:mysql://localhost:3306/textbasedadventure","root","")
	val statement = connection.createStatement

	//Global, important variables
	var player = ""
	var death = true
	var place = ""
	//

	//QueryList
	//var itemPriceQuery = "Select price FROM ItemDB where name = item"
	var selectRewardsQuery = "Select Rewards FROM QuestsDB"
	//

	//RANDOM functions	

	//Chooses a vendor
	/*def chooseVendor(vendor:string):String{
		vendor match{
			case "Beena" =>
		}*/

	//}
	//Fills inventory with empty slots
	def fill(num:Int,array:Array[String]){
		for (k <- 0 to array.length-1){
			array(k) = "empty"
		}

	}

	//Choose who's playing
	def choosePlayer()={ 
		var pend = true
		while(pend){
			println("Who is playing?")
			player = readLine().toLowerCase()
			player match{
				case "chester" => player = "Chester" 
					CS = ChesterCS
				pend = false
				case "josh" => player = "Josh"
					CS = JoshCS
				pend = false
				case "chris" => player = "Chris"
				pend = false
				case "mac" => player = "Mac"
				pend = false
				case _ => println("Please use your real name, at least for now")
			}
			CS.Attributes.makeAttributes(player)
			var dropReadyMade = statement.executeUpdate(s"DROP TABLE $player")
			var createTableQuery = s"CREATE TABLE $player (Name char(30), Rank Int(3), Description char(155), `Bonus per Rank/per Lvl` char(55), Chant char(105))"
			var createTable = statement.executeUpdate(createTableQuery)
			var updateQuery = (s"INSERT INTO $player (Name, Rank, `Bonus per Rank/per Lvl`, Chant, Description) SELECT Name, Rank, `Bonus per Rank/per Lvl`,Chant, Description FROM Spells") //WHERE Name = `Lightning Zap`")
			var update = statement.executeUpdate(updateQuery)
			

		}
	}
	//

	//Finds string within Player's response -> for choices
	def findString(find:String,choice2:String): Boolean={
		var i = 0
		while (i != choice2.length){
			if (i+find.length > choice2.length){
				return false
			}
			if (choice2.substring(i,i+1) == find.substring(0,1)){//Out of Range as always
				if (choice2.substring(i,i+find.length) == find){
					return true
				}	
			}
			i += 1
		}
		return false
	}
	//

	//Need function to find index of certain bit of string _> For QUEST stuff, need to get 2nd index too
	def stringIndex(string:String,start:String): Int={
	 	for (k <- 0 to string.length-1){
	 		if (string.substring(k,k+1) == start){
	 			return k
	 		}
	 	}
	 	return string.length-1
	}

	//

	//MoneyPrinting
	def moneyPrint(num:Int):String={
		var total = num
		var string = ""
		if (total/100 > 0){
			string += ((total/100)+" gold ")
			total = total - (100*(total/100))
		}
		if (total/10 > 0){
			string += ((total/10)+" silver ")
			total = total - (10*(total/10))			
		}
		if (total > 0){
			string += (total+" copper ")
		}
		
		return string
	}
	//

	//'Gives' quest to characters
	def giveQuest(questName:String){
		statement.executeUpdate(s"UPDATE QuestsDB SET Acquired = 1 WHERE Title = '$questName'")
	}
	//
	//
	//CLASSES

	


	//inventory class
	class Inventory{

		var inventory =  new Array[String](15)
		fill(15,inventory)
		private var Inumber = 0

		def checkInumber():Int={//checks the most empty inventory slot
			for (k <- 0 to inventory.length-1){
				if (inventory(k) == "empty"){
					return k
				}
			}
			return (inventory.length -1)
		}

		def Iadd(num:Int,what:String)={//adds into the inventory in the appropriate empty slot

			inventory(Inumber) = (s"$what x$num").toLowerCase
			Inumber = checkInumber()

			var checkDBQuery=(s"Select count(*) as Count FROM ItemDB WHERE Name = '$what'")
			var checkDB = statement.executeQuery(checkDBQuery)
			checkDB.next()
			if (checkDB.getInt("Count") == 0){
				statement.executeUpdate(s"INSERT INTO ItemDB VALUES('$what',1,1)")
						//All items not in main Itemdb (japtem)Beena(1) will buy and sell for 1
				}
		}

		def Iremove(num:Int,what:String)={//removes from the inventory from the appropriate slot
			var number = 0
			Inumber = checkInumber()
			for (k <- 0 to inventory.length-1){
				if(findInventString(k,what)){
					var quantity = CS.Inventory.inventory(k).substring(stringIndex(CS.Inventory.inventory(k),"x")+1).toInt
					 number = quantity-num
					inventory(k) = "empty"
					
				}
			}
			if (number > 0){
				Iadd(number,what)
			}
			
		}
	}

	//Character Sheet class
	class CharacterSheet{
		var Inventory = new Inventory()
		var Money = 10
		var Attributes = new Attributes()
		var Combat = new Combat()
	}


	//chooseAttributes function
	
	//Attributes class
	class Attributes{
		var JoshCharacterAttributes = Array(1,1,4,25,2,1,0,4,0,-2)
		var ChesterCharacterAttributes = Array(2,7,3,9,2,4,1,7,2,1)
		var MacCharacterAttributes = Array(2,0,2,15,0,0,0,8,0,2)
		var ChrisCharacterAttributes = Array(3,0,4,25,0,1,1,10,0,3)
		var Attributes = Array(0,0,0,0,0,0,0,0,0,0)

		def chooseAttributes(player:String,Attribute:Int):Int={
			player.toLowerCase match{
				case "josh" => return JoshCharacterAttributes(Attribute)
				case "chester" => return ChesterCharacterAttributes(Attribute)

				case "chris" => return ChrisCharacterAttributes(Attribute)
				case "mac" => return MacCharacterAttributes(Attribute)
				case _ => return 0
				}
		}
		def printAttributes()={
			println("Might: "+Might)
			println("Mana: "+Mana)
			println("Speed: "+Speed)
		}

		def makeAttributes(player:String)={
			Might = chooseAttributes(player,0)
			Mana = chooseAttributes(player,1)
			Speed = chooseAttributes(player,2)
		 	Hits = chooseAttributes(player,3)
			Craft = chooseAttributes(player,4)
		 	Learning = chooseAttributes(player,5)
			Defense = chooseAttributes(player,6)
			Luck = chooseAttributes(player,7)
			Social = chooseAttributes(player,8)
			Repuation = chooseAttributes(player,9)
		}

		var Class = "None"
		var Race = "Human"

		var Might = 0
		var Mana = 0
		var Speed = 0
		var Hits = 0
		var Craft = 0
		var Learning = 0
		var Defense = 0
		var Luck = 0
		var Social = 0
		var Repuation = 0

	}
	//
	//Combat class
	class Combat{
		def attack()={
			println("Attack")
		}
	}


	//

	var CS:CharacterSheet = new CharacterSheet()
	//Making Characters
	var ChesterCS = new CharacterSheet()
	ChesterCS.Inventory.Iadd(1,"Dryad Arm")
	ChesterCS.Inventory.Iadd(2,"Basket of Beans")
	ChesterCS.Inventory.Iadd(4,"Spears")
	println(ChesterCS.Inventory.inventory.length)
	var JoshCS = new CharacterSheet()
	//
	//certain String Index function

	

	//

	//Buy and Sell functions
	def Buy(Vendor:String)={ //Finds vendors stock and prints it
		//Vendor(Vendor)
		println("For Sale:")
		var vendorDB = ""
		
		Vendor match{//gets the appropriate Vendor DB
			case "Beena"=> vendorDB = "Bitems"			
			case "Fergo"=> vendorDB = "Fitems"		
			case "Marroon"=> vendorDB = "Mitems"
		}
		var vendorStockQuery = (s"select * from $vendorDB")
		var printVendorStock = statement.executeQuery(vendorStockQuery)
				println("Item,Price")
				while(printVendorStock.next()){//Prints a list of the items and prices
					var name = printVendorStock.getString("Name")
					var price = moneyPrint(printVendorStock.getInt("Price"))
					println(name+"  "+price)
				}
		
		println("What do you want to buy?")
		var choiceBuy = readLine().toLowerCase
			
		var compareVendorStock = statement.executeQuery(vendorStockQuery)
		var end = true
		var p = 0
		var s = 0
		var n = ""
		var buyAmount = 0 
		while(compareVendorStock.next()&& end){
			println("Start")
			 n = compareVendorStock.getString("Name")
			if (findString(n.toLowerCase,choiceBuy)){
				p = compareVendorStock.getInt("Price")
				s = compareVendorStock.getInt("Stock")
				println(s"I have $s $n in stock. how much do you want to buy?")
				buyAmount = readInt()
				end = false
				
				//if CS money too little do something
				if (buyAmount > s){
					buyAmount = s
					println("Guess you want all of it then")
				}
				while(CS.Money < p*buyAmount){
					println("Erm you only have "+moneyPrint(CS.Money)+". You should buy a little less.")
					buyAmount = readInt()
				}

				//else:
			}
		}
		println(s"That will be "+moneyPrint(p*buyAmount))
		s -= buyAmount
		statement.executeUpdate(s"UPDATE $vendorDB SET Stock = '$s' where Name = '$n'")//updates the db with the stock change

		
	}

	//cheeky find string miniturized
	def findInventString(k:Int,sell:String): Boolean={
		return (findString(CS.Inventory.inventory(k).substring(0,stringIndex(CS.Inventory.inventory(k),"x")-1),sell))
	}
	def Sell()={
		var end = true
		while (end){
			var actualLength = 0
			println("You have:")
			for (i <- 0 to CS.Inventory.inventory.length-1){
				if (CS.Inventory.inventory(i) != "empty"){
					println(CS.Inventory.inventory(i))
					actualLength +=1
				}
			}
		

			println("What would you like to sell")
			var sell = readLine().toLowerCase

			/*var sellArray = new Array[String](actualLength)
			var A = 0
			if (findString(" ",sell)){
				println("Hello1")
				for (k <- 0 to actualLength-1){
					var B = stringIndex(sell," ")
					sellArray(k) = sell.substring(A,B)
					A = B
				}
			}
			else{
				sellArray(0) = sell
			}
			for (h <- 0 to sellArray.length-1){
				println("hell "+sellArray(h))*/

				for (k <- 0 to actualLength-1){
					println(CS.Inventory.inventory(k))
					if (findInventString(k,sell)){
						var item = (CS.Inventory.inventory(k).substring(0,stringIndex(CS.Inventory.inventory(k),"x")-1)) //Only gets the name
						println("Item: "+item)
						var quantity = CS.Inventory.inventory(k).substring(stringIndex(CS.Inventory.inventory(k),"x")+1) // only gets the quantity
						var itemPriceQuery = (s"Select Price FROM ItemDB where Name = '$item'")
						var itemPrice = statement.executeQuery(itemPriceQuery)
						itemPrice.next()
						var iP = itemPrice.getInt("Price")
						println(s"You have $quantity of them. How many do you want to sell?")
							var sold = readInt()
						CS.Inventory.Iremove(sold,item)//lowers/removes item quantity
						println(s"You sold $sold $item for "+(iP*sold)) //Should make a printMoney() function to show gold,silver,copper
						CS.Money += (iP*sold)
						println("Do you want to sell anything else")
						var ending = readLine.toLowerCase
						ending match{
							case "n"|"no" => end = false
							case _ => end = true
						}
					}
				}
			//}
		}
		
	}

	//

	//Quest functions
	//Get Rewards from Quest
	def findReward(string:String,character:String):String={//Finds certain substring from the main Rewards string
		var charaOne = stringIndex(string,character)
		var charaTwo = stringIndex(string.substring(charaOne+1),character)+charaOne
		var stringFound = string.substring(charaOne+1,charaTwo+1)
		return stringFound
	}

	var Qrewards = statement.executeQuery("Select Rewards FROM QuestsDB")
	
	def splitRewardItems(string:String,character:String)={
		var A = 1
		var end = true
		while (end){
			//string
			//println("StringIndex??? "+stringIndex(string.substring(A),","))
			var checkIndex = stringIndex(string.substring(A),",")
			if (checkIndex == 0){
				A+=1
			}
			//println("Aa"+A)
			//println("String "+stringIndex(string.substring(A),"x"))
			var stringString = string.substring(A,stringIndex(string.substring(A),"x")+A)
			var theSeparator = stringIndex(string.substring(A),",")


			var numString = ""
			
			
			if (theSeparator == 0){
				theSeparator = stringIndex(string.substring(A),",")
			}
			//println("theSeparator "+theSeparator)
			var stringLength = (string.substring(A).length)
			
			println("Stringstring "+stringString)
			println(A+"A")
			if (theSeparator != stringLength-1 ){
				numString = string.substring(stringIndex(string,"x")+1,stringIndex(string,","))
			}
			else{
				numString = string.substring(stringIndex(string.substring(A),"x")+1,stringIndex(string.substring(A),")"))
				end = false
			}
			A = theSeparator+1
			println("numString "+numString)
			println("End"+end)
			CS.Inventory.Iadd(numString.toInt,stringString)
			//string = (foodx1,drinkx1)
			//newString = foodx1
			//Iadd(1,food) <- shouldget	
		}
	}

	def checkQuests(questName:String):Boolean={
		var questAcquiredQuery = s"SELECT count(*) AS Count FROM QuestsDB WHERE Title = '$questName' AND Acquired = 1"
		var questAcquired = statement.executeQuery(questAcquiredQuery)
		questAcquired.next()
		var Acquired = questAcquired.getInt("Count")

		if (Acquired == 1){
			return true//Should complete the get out quest
		}
		return false
	}

	def checkProgressMade(questName:String):Int={
		var questProgressMadeQuery = s"SELECT ProgressMade FROM QuestsDB WHERE Title = '$questName'"
		var questProgressMade = statement.executeQuery(questProgressMadeQuery)
		questProgressMade.next()
		var ProgressMade = questProgressMade.getInt("ProgressMade")
		return ProgressMade
	}
	def checkProgress(questName:String):Int={
		var questProgressQuery = s"SELECT Progress FROM QuestsDB WHERE Title = '$questName'"
		var questProgress = statement.executeQuery(questProgressQuery)
		questProgress.next()
		var Progress = questProgress.getInt("Progress")
		return Progress
	}

	def QuestRewards(questName:String)={

		//var getRewardQuery = "SELECT * FROM QuestsDB"
		//var getReward = statement.executeQuery(getRewardQuery)
		var questRewardQuery = (s"SELECT Rewards FROM QuestsDB WHERE Title = '$questName'")
		var questRewardList = statement.executeQuery(questRewardQuery)
		questRewardList.next()
		var questRewards = questRewardList.getString("Rewards")

		if (findString(")",questRewards)){//Items //Index out of ranger -_-
			println("Here??")
			splitRewardItems(questRewards,",")
		}
		if (findString("@",questRewards)){//Money
			CS.Money += (findReward(questRewards,"@").toInt)
			print(CS.Money)
			}
		if (findString("#",questRewards)){//Bonus Rewards (Items for now, change later)
			CS.Inventory.Iadd(1,findReward(questRewards,"#"))
			//findReward is find certain chracter string
		}
		for (k <- 0 to 5){
			println(CS.Inventory.inventory(k))
		}
	}

	def QuestUpdate(questName:String)={ //Updates the Quest progress, minusing until it reaches 0 (completion)
		var questTitleQuery = (s"SELECT Title, Progress FROM QuestsDB WHERE Title = '$questName'") 
		var questTitleList = statement.executeQuery(questTitleQuery)
		questTitleList.next()
		var questTitle = questTitleList.getString("Title")
		if (questName == questTitle){
			println(questTitle)
		}
		var questProgressMadeQuery = (s"SELECT  ProgressMade FROM QuestsDB WHERE Title = '$questName'")
		var questProgressMadeList = statement.executeQuery(questProgressMadeQuery)
		questProgressMadeList.next()
		var questProgressMade = questProgressMadeList.getInt("ProgressMade")
		statement.executeUpdate(s"UPDATE QuestsDB SET Progress = ProgressMade+1 WHERE Title = '$questName'")

		
		//if (questProgress <= 0){
			//println("I MADE IT")
			//QuestComplete(questName)
		//}
	}

	def questType(questName:String):String={
		var questTypeQuery = s"SELECT Type FROM QuestsDB WHERE Title = '$questName'"
		var questType = statement.executeQuery(questTypeQuery)
		questType.next
		var Type = questType.getString("Type")
		return Type
	}
	def QuestComplete(questName:String)={
		QuestRewards(questName)
		questType(questName) match{
			case "Singular"|"Hidden" => statement.executeQuery(s"DELETE FROM QuestsDB WHERE Title= '$questName'")
			case "Repeatable" => statement.executeUpdate(s"UPDATE QuestsDB SET Acquired = 0, ProgressMade = 0 WHERE Title = '$questName'")
		}
		
		//Change QuestsDB to have a progress and 'tasks' column (so its what it goes back up to if its not a singular quest)
		//SQLQuestName.progress = SQLQuestName.tasks
	}

	//




	//
	
	//LOCATIONS

	def Street()={
		println("Up to a few seconds ago you were busy contemplating what amazing name your character was going to have.")
		println("Whether they'd be an elf, evil, ugly, yada yada. sadly you are but yourself.")
		println("\n")
		println("A small line of information floats above your head")
		playerActions()
		println("Do not forget, whenever making a decision, you can type 'esc' to access those options")
		var end = true
		while (end){
			println("What would you like to do? Go to: (Beenas/Tavern/Outside/Smithy or stay where you are...in the street like a dolt")
			var choice1 = readLine().toLowerCase()
			if (choice1 == "esc"){
				playerActions()
			}
			else{
				place = choice1.substring(0,1)
				end = false
			}
		}
	}

	//Beena's Emporium encounter
	def Beenas()={
		println("Her door is open and you head inside to see a young woman in her early thirties mumbling to herself lost in thought")
		println("but before you can react her head whips towards you, a smile already encroached upon her face")
		println("'HELLLLLLLLOOOOO WELCOME TO BEEEENAS' she almost jumps onto you in excitement 'What can i do for you today")
		var end = true
		while (end){
			println("[Buy,Sell,Talk,Leave]")
			var choice2 = readLine().toLowerCase
			if (choice2 == "esc"){
				playerActions()
			}
			if(findString("buy",choice2)){
				println("'OH, what would you like to buy? I've got Beans of multiple varieties, healing salves, pelts and certain knicknacks just. for. you!'")
				//give player buy options
				Buy("Beena")
			}
			else if (findString("sell",choice2)){
				println("'I'd prefer you'd buy but my store could always use some stocking up, what have you got?")
				Sell()
				//check player's 'inventory' and offer prices for them
			}
			else if((findString("help",choice2)||findString("talk",choice2))||findString("quest",choice2)){
				println("No buying anything? oh well guess we can chat")
				if(checkQuests("Beanfield")){
					if (checkProgressMade("Beanfield") >= checkProgress("Beanfield")){
						println("'OH Thank YOU!'")
						QuestComplete("Beanfield")
					}
				}
				else{
					println("'Well you know me, I always need some beans a picking")
					giveQuest("Beanfield")
					//adds bean picking to quests
				}
				if (player == "Chester"){
					println ("*wink*")
				}

			}
			else if ((findString("leave",choice2)||findString("goodbye",choice2))||findString("bye",choice2)){
				println("Until later!, but you know come back soon!!!")
				end = false
				
			}
			else{
				println("'Hello, anybody in there? yes, you, can you stop staring off into the abyss in my shop please'")
			}
		}
		Street()
	}
	//

	//Fergo's Smithy encounter
	def Smithy()={
		println("Walking across the small village you quickly end up at the Smithy who's forges are blazing in tune to the ringing of metal being hammered into shape")
		println("As you step around to the wide open entrance, you're greeting by the half-naked sight of a burly man in his late fifties, only a grand handlebar moustache gracing his otherwise bald body")
		println("Fergo does not notice you, do you want to grab his attention")
		var end1 = true
		while(end1){
			println("[Yes,No]")
			var choice2 = readLine().toLowerCase()
			choice2.substring(0,1) match{
				case "e" => playerActions()
				case "n" => println("Alright, you stand there twiddling your thumbs")
				case "y" => println("Making some form of noise/movement, you get Fergo to stop hammering away for a moment, thus noticing your presence")
					println("'G'morn, what you want?'")
					var end = true
					while (end){
						println("[Buy,Sell,Talk,Leave]")
						var choice3 = readLine().toLowerCase()
						if (choice3 == "esc"){
							playerActions()
						}
						if (findString("buy",choice3)){
							println("'Looking to protect yerself or fuck something up?'")
							Buy("Fergo")
							//give player buy options
						}
						else if (findString("sell",choice3)){
							println("'Well don't see what you could have to offer, but i'm never one to not have a peek first'")
							Sell()
							//check players inventory and offer prices for them
						}
						else if((findString("help",choice3)||findString("talk",choice3))||findString("quest",choice3)){
							if(checkQuests("Minefield")){//Checks if characters have acquired it
								if (checkProgressMade("Minefield") >= checkProgress("Minefield")){
									println("'Good'")
									QuestComplete("Minefield")
								}
							}
							else{
								println("'I'm always in need for some more Iron, same deal as before'")
								giveQuest("Minefield")
								//Add MineField Quest to Active Quests
							}
						}
						else if (findString("leave",choice3)||findString("goodbye",choice3)||findString("bye",choice3)){
							println("Aight whatever, bye")
							end = false
						}
						else{
							println("He stares at you hard for a moment while you daydream, before going back to hammering away")
						}
					}
			}
		}
		Street()
	}
	//

	//Marroon and Meyra's Tavern
	def Tavern()={
		println("The hot air wafts from the kitchen accompanied a cacaphony of noises which could only be taken as the murdering of a kitten")
		println("and you notice MARROON peering at you from behind the bard, rubbing what you swear is the same tankard as from three days ago with the same dishcloth")
		println("What would you like to do?")
		var end = true
		while(end){
			println("[Eat/Drink,Talk,Go Upstairs,Go Downstairs,Leave]")
			var choice2 = readLine().toLowerCase()
			if (choice2 == "esc"){
				playerActions()
			}
			if (findString("drink",choice2) || findString("eat",choice2)){
				println("MARROON, almost knowingly, produces a steaming plate of mashed meat and beans along with a tankard, placing it on the counter for you")
			}
			else if (findString("talk",choice2)){
				var c3end = true
				while (c3end){
					println("Who would you like to talk to, you can see Marroon and you suspect Meyra is in the kitchen")
					println("[Marroon,Meyra]")
					var choice3 =readLine().toLowerCase()
					if (choice3 == "esc"){
						playerActions()
					}
					choice3.substring(0,2) match{
						case "ma" => println("MARROON says hi with a slow nod") 
						c3end = false
						case "me" => println("You brave the kitchens to find MEYRA")
							println("BUT MEYRA FINDS YOU! Actually she almost bumps into you rushing out of the kitchen with her usual charging stomp")
							println("'What are you doing here!?!'")
							var end2 = true
							while (end2){
								println("[Buy,Sell,Talk]")
								var choice4 = readLine().toLowerCase()
								if (choice4 == "esc"){
									playerActions()
								}
								if (findString("buy",choice4)){
									println("Really? For flips sake")
									Buy("Marroon")
								}
								else if (findString("sell",choice4)){
									println("If its another half rotten goblin I swear to Kresreb I'll shove it so far up")
									Sell()
								}
								else if ((findString("talk",choice4)||findString("quest",choice4))||findString("help",choice4)){
									println("You know what you could actually help me")
									giveQuest("GET OUT")
								}
								else if ((findString("leave",choice4)||findString("goodbye",choice4))||findString("bye",choice4)){
									println("Halleluliagh")
									if(checkQuests("GET OUT")){//Checks if characters have acquired it
										QuestUpdate("GET OUT")
										if (checkProgressMade("GET OUT") >= checkProgress("GET OUT")){
											QuestComplete("GET OUT")
										}
									}
									end2 = false
								}
								else{
									println("Meyra is having none of that so she picks you up and tosses you out")
									if(checkQuests("GET OUT")){//Checks if characters have acquired it
										QuestUpdate("GET OUT")
										if (checkProgressMade("GET OUT") >= checkProgress("GET OUT")){
											QuestComplete("GET OUT")
										}
									}
									end2 = false
								}
							}

						 c3end = false

						case _ => println("I have no idea who you're talking to, but thin air is not the best social companion")
					}
				}	
			}
			else if (findString("Go upstairs",choice2)){
				println("You head upstairs")
			}
			else if (findString("Go downstairs",choice2)|| findString("cellar",choice2)){
				println("You head into the cellar")
			}
			else if ((findString("leave",choice2)||findString("goodbye",choice2))||findString("bye",choice2)){
				println("*grunt*")
				end = false
			}
			else{
				println(s"you $choice2...great")
			}
		}
		Street()
	}
	//
//
	//Outside Areas
	//Outside 'Street'
	def Outside()={
		println("You head outside Pueblo and the fresh, foresty air quickly surrounds you. Where are you planning to go?")
		var end = true
		while (end){
			println("[Forest,Mountains,Ravine,Town]")
			var choice1 = readLine().toLowerCase
			if (choice1 == "esc"){
				playerActions()
			}
			else{
				place = choice1.substring(0,1)
				end = false
			}
		}
		//create array of possible options based on quests given???
	}
	//
	//Mountains
	def Mountains()={
		println("Mountains")
	}

	def Ravine()={
		println("Ravine")
	}

	def Forest()={
		println("You head into the forest but where are you actually going?")
		var end = true
		while (end){
			println("[Walk,Quest,Town]")
			var choice1 = readLine().toLowerCase()
			if (choice1 == "esc"){
				playerActions()
			}
			if (findString("quest",choice1)){
				println("What quest are you on? You should check your Quest Log(esc->q) if you don't remember the names!")
				var end2 = true
				while (end2){
					var choice2 = readLine().toLowerCase()
					if (choice2 == "esc"){
						playerActions()
					}
					else{
						choice2.substring(0,2) match{
							case "be" => Beanfield()
							case "go" => Goblins()
							case "mi" => Minefield()
							case "ba" => Bagel()
						}
						end2 = false
					}
				}
			}
			else if (findString("town",choice1)){
				println("You've braved the forests enough for now you decide! And then scamper off back to Pueblo")
				place = "street"
				end = false
			}
			else{
				blackDeer()
				randomEncounters()
			}	
		}
	}

	//RANDOM ENCOUNTERS 
	def randomEncounters()={
		println("Random Encounter!")
		var roll = diceRoll()
		roll match{
			case 2|3 => println("The birds tweet")
			case 4|5 => berryField()
			case 6|7|8 => Combat("Goblin",2,1,4,0)
			case 9|10 => dryadTree()
			case 11 => blackDeer()
			case 12 => Combat("Badger",22,4,4,2)
		}


	}

	def berryField()={
		println("Berry Field")
	}

	def dryadTree()={
		println("The Birds are tweeting, badgers are sleeping and you are walking straight into a thick black-barked tree")
		println("Are you going to stop, go around it or just barrel right into it?")
		var end = true
		while(end){
			println("[Stop, Go Around, Go Through]")
			var choice1 = readLine().toLowerCase()
			if (choice1 == "esc"){
				playerActions
			}
			else{
				if (findString("stop",choice1)||findString("stay",choice1)){
					println("You stop walking and look at...suprise, a tree.")
				}
				else if (findString("around",choice1)){
					println("You walk past the tree...cool")
					end = false
				}
				else if (findString("through",choice1)){
					Combat("Dryad",14,3,2,2)
					end = false
				}
			}
		}
	}

	def blackDeer()={
		println("Losing yourself in the forest, you find yourself in a glade where a large black doe languidly lies on the glinting grass")
		println("It notices your approach but doesn't seem to care")
		println("What will you do?")
		var end = true
		while(end){
			println("[Attack,Pet,Talk,Leave]")
			var choice1 = readLine().toLowerCase()
			if (choice1 == "esc"){
				playerActions()
			}
			else if (findString("attack",choice1)){
				println("You heartless bastard! she was just basking in the sun being harmless!")
				println("She bounds up to her feet and faces you off")
				Combat("Black Doe",8,1,5,0)
			}
			else if (findString("talk",choice1)){
				println("What are you saying to her?")
				var choice2 = readLine()
				println("It bleats back...what did you expect, it's a deer.")

			}
			else if (findString("leave",choice1)){
				println("You leave the glade")
				end = false
			}
			else if (findString("pet",choice1)){
				println("she rolls onto her knees, looking up at you")
				println("[Pet,Cuddle,Feed,Leave]")
				var end2 = true
				while(end){
					var choice2 = readLine().toLowerCase()
					if (choice2 == "esc"){
						playerActions()
					}
					else if (findString("pet",choice2)){
						blackdeer.kiss()

					}
					else if (findString("cuddle",choice2)){
						blackdeer.cuddle()
					}
					else if (findString("feed",choice2)){
						println("You feed her a tuft of grass from nearby")
						blackdeer.lick()
					}
				}
			}
		}
	}

	//QUEST ENCOUNTERS
	def Beanfield()={
		println("Beanfield")
	}
	def Goblins()={
		println("Goblins")
	}
	def Minefield()={
		println("Minefield")
	}
	def Bagel()={
		println("Bagel")
	}

	//random functions

	def playerActions()={
		var end = true
		while (end){
			println("Available Actions: I:inventory, C:character, Q:questLog, S:spellsList F:interact with world")
			var choice = readLine
			choice.substring(0,1).toLowerCase match{
				case "i" => 
					for (k<-0 to CS.Inventory.inventory.length-1){
						println(CS.Inventory.inventory)
					}
				case "c" => println ("You're looking pretty good")

				case "q" => 
					var questAllQuery = "SELECT * FROM QuestsDB WHERE Acquired = 1"
					var questAll = statement.executeQuery(questAllQuery)
					println("Title..................Task")
					println("---------------------------")
					while(questAll.next()){
						var questTitle = questAll.getString("Title")
						var questTask = questAll.getString("Task")
						println(questTitle+"........."+questTask)
					}
				case "s" =>
					var getAllSpellsQuery = s"SELECT * FROM $player"
					var getAllSpells = statement.executeQuery(getAllSpellsQuery)
					println("Name:...............Description................................................[Chant]")
					println("--------------------------------------------------------------------------------------")
					while(getAllSpells.next()){
						var spellsName = getAllSpells.getString("Name")
						var spellsDescription = getAllSpells.getString("Description")
						var spellsChant = getAllSpells.getString("Chant")
						println(spellsName+"......"+spellsDescription+"........["+spellsChant+"]")
					}
				case "f" => end = false
			}
		}
	}
	//

	//Animal Cuddling Encounter Class
	abstract class Animal{
		def bleat
	}

	trait Cuddly {
		def cuddle
		def kiss
		def lick
	}

	class Deer extends Animal with Cuddly{
		override def bleat()={
			println("Bark")
		}
		override def cuddle()={
			println("she nuzzles your hand")
		}
		override def kiss()={
			println("She gives you a wet boop with her nose")
		}
		override def lick()={
			println("she licks your hand")
		}

	}

	var blackdeer = new Deer()




	//Combat function
	def diceRoll():Int={
		var randInt = scala.util.Random
		var roll = randInt.nextInt(11)+2//does random Int from 2(2d6 means min 2) to 12 (13 not included)
		return roll
	}
	def combatDiceRoll(name:String):Int={
		var roll = diceRoll()
		println(s"$name Rolled a "+roll)
		return roll
	}
	def Combat(enemyName:String,enemyHits:Int,enemySpeed:Int,enemyAttack:Int,enemyDefense:Int):String={
		var eHits = enemyHits
		var combat = true
		println("*Combat music plays*")
		println("What are you going to do!")
		while (combat){
		println("You have:"+CS.Attributes.Hits+" Hits")
		println(s"$enemyName has: $eHits Hits")
		println("[Attack,Spell,Flee]")

			var choice1 = readLine.toLowerCase
			if (choice1 == "esc"){
				playerActions()
			}
			else if (findString("attack",choice1)){
				//println("You have: "+CS.Attributes.Hits+" Hits")
				//println("Enemy has:"+eHits+" Hits")
				eHits -= attack(enemyName,enemyAttack,enemyDefense)
			}
			else if (findString("spell",choice1)){
				println("What do you chant?")
				var chant = readLine()
				var spellNameQuery = s"SELECT Name FROM $player WHERE Chant = '$chant'"
				var spellName = statement.executeQuery(spellNameQuery)
				spellName.next()
				var name = spellName.getString("Name")
				var cast = getCombatSpellAbility(chant)
				if (cast == 0){
					println("Your spell fizzles out..maybe you didn't say it right")

				}
				else{
					println(s"Kablam, you cast $name for $cast damage")
				}
				eHits -= attack(enemyName,enemyAttack,enemyDefense)

			}
			else if (findString("flee",choice1)){
				if (CS.Attributes.Speed - enemySpeed >= 0){
					combat = false
					return "You got away!"
				}
				else{
					println(s"You tried but the $enemyName caught up to you")
					eHits -= attack(enemyName,enemyAttack,enemyDefense)
				}
			}

			if (CS.Attributes.Hits <= 0){
				combat = false
				println("You have died")
				death = false
				return "Lost"
			}
			else if (eHits <= 0){
				combat = false
				println(s"VICTORY! You killed a $enemyName")
				return "Won"
			}
			println("\n")
		}
		return "Unconcluded"
	}
	//
	//Attack function
	def attack(enemyName:String,enemyAttack:Int,enemyDefense:Int):Int={
		var Attack = combatDiceRoll("You") + CS.Attributes.Might - (enemyAttack+combatDiceRoll(enemyName))
		var Dmg = Attack - enemyDefense
		var DmgTaken = -(Attack + CS.Attributes.Defense)
		//println("Dmg to take:"+DmgTaken)
		if(Attack < 0){//So the Enemy's attack was higher than the players
			if(DmgTaken > 0){//if its 0 or less player takes no dmg
				CS.Attributes.Hits -= DmgTaken
				println("You took: "+DmgTaken+" Damage")
				return 0
			}
		}
		else if (Attack > 0){//players attack higher
			if(Dmg > 0){
				println(s"You hit $enemyName for: "+Dmg+" Damage")
				return Dmg
			}
		}
		return 0
	}


	//SPELL STUFF
	def checkSpells()={//checks for players combat spells to use in combat
		var checkSpellsQuery = s"SELECT * FROM Spells WHERE Name IN(SELECT Name FROM '$player')"
		var checkSpells = statement.executeQuery(checkSpellsQuery)
		println("Name:.....Description:......................Chant:")
		println("----------------------------------------------------")
		while(checkSpells.next()){
			var spellName = checkSpells.getString("Name")
			var spellDescription = checkSpells.getString("Description")
			var spellChant = checkSpells.getString("Chant")
			println(spellName+"..."+spellDescription+"...["+spellChant+"]")

		}
	}

	def getCombatSpellAbility(chant:String):Int={
		var getSpellAbilityQuery = s"SELECT Name, Rank, `Bonus per Rank/per Lvl`, count(*) AS Count FROM Spells where Chant = '$chant'"
		println ("chant: "+chant)
		var getSpellAbility = statement.executeQuery(getSpellAbilityQuery)
		getSpellAbility.next()
		var dmg = 0
		var spellAbility = getSpellAbility.getString("Bonus per Rank/per Lvl")
		var spellRank = getSpellAbility.getInt("Rank")
		var spellCount = getSpellAbility.getInt("Count")
		var spellName = getSpellAbility.getString("Name")
		if (spellCount != 0){
			dmg = spellAttack(spellAbility,spellRank)
		}
		return dmg
	}

	def spellAttack(ability:String,rank:Int):Int={
		var Lvl = rank
		var remainder = 0
		var randInt = scala.util.Random
		var slash = stringIndex(ability,"/")
		var perRank = ability.substring(0,slash)
		var perLvl = ability.substring(slash+1)
		var dmg = 0
		var roll = 0
		if (Lvl >= 5){
			remainder = Lvl - Lvl%5
			if (findString("d",perRank)){
				var d = stringIndex(perRank,"d")
				var numberOfDice = perRank.substring(0,d).toInt
				var sizeOfDice = perRank.substring(d).toInt
				roll = randInt.nextInt(((sizeOfDice*numberOfDice)-numberOfDice+1))+numberOfDice
			}
			else{
				roll = perRank.toInt
			}
		}
		if (findString("+",perLvl)){
			roll += (perLvl.substring(1).toInt * Lvl)
		}
		else if (findString("-",perLvl)){
			roll -= (perLvl.substring(1).toInt * Lvl)
		}

		return roll
	}
	

	//
	//Main code bit
def main():Unit={
	choosePlayer()
	//testing area
	//testing area 
	//Player chooses actions
	Combat("Goblin",2,1,1,0)
	
	while (death){
		place match{
			case "t" => Tavern()
			case "b" => Beenas()
			case "s" => Smithy()
			case "o" => Outside()
			case "f" => Forest()
			case "m" => Mountains()
			case "r" => Ravine()
			case _ => Street()
		}
	}
}
main()
