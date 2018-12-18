
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

	def QuestUpdate(questName:String)={ //Updates the Quest Progress Made, minusing until it == Quest Progress(completion)
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
				println("You've braved the forests enough for now, you decide! And then scamper off back to Pueblo")
				place = "street"
				end = false
			}
			else{
				randomEncounters()
			}	
		}
	}

	//RANDOM ENCOUNTERS 
	def randomEncounters()={
		println("Random Encounter!")
		var roll = diceRoll()
		println("you rolled a "+ roll)
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
		println("Wandering aimlessly through the forest for a while, you end up walking straight into a small glade filled with numerous berry bushes")
		println("What would you like to do?")
		var end = true
		while (end){
			println("[Pick,Ignore]")
			var choice1 = readLine().toLowerCase()
			if (choice1 == "esc"){
				playerActions()
			}
			var roll2 = diceRoll()
			if (findString("leaves",choice1)){
				println("Thanks to Beena you know to pick the leaves")
				println("You pick "+ roll2+ "red-berry leaves")
				CS.Inventory.Iadd(roll2,"Healing Leaves")
				
			}
			else if (findString("pick",choice1)){
				var roll = diceRoll()/3
				
				roll match{
					case 1 => println("You pick "+ roll2+ "blue-berries")
						CS.Inventory.Iadd(roll2,"blueBerries")
					case 2 => println("You pick "+ roll2+ "red-berries")
						CS.Inventory.Iadd(roll2,"redBerries")
					case 3 => println("You pick "+ roll2+ "yellow-berries")
						CS.Inventory.Iadd(roll2,"yellowBerries")
					case 4 => println("Sadly the bushes seem to have been picked...sucks for you")
				}
			}
			else{
				println("You decide to walk away without touching the bushes")
			}
			end = false
		}
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
					println("Uncaring of the warnings from the narrator, you barrel straight into the tree...or should I say a very miffed Dryad")
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
		println("Following Beena's instructions, you go over the hill and under the cave and find yourself at a field of trellised beanstalks")
		println("What are you going to do")
		var end = true
		var quest = 0
		while (end){
			println("[Pick, Walk, Leave]")
			var choice1 = readLine().toLowerCase()
			if (choice1 == "esc"){
				playerActions()
			}
			if (findString("pick",choice1)){

			}
			else if (findString("explore",choice1)|| findString("walk",choice1)){
				println("You walk around and...see a lot of beanstalks trellised in neat little lines")
				var roll = diceRoll()/4
				
				roll match{
					case 1|2 => println("You pick some beans")
						quest += 1
					case 3 => println ("the beanstalk fights back!!")
						Combat("Beanstalk",8,4,2,0)
						quest += 1

				}
			}
			else if (findString("sing",choice1)){
				println("Opening you mouth to release a chorus of broken notes....seems to do the job?")
				println("And after plucking up your courage to pick the beans...you collect a sufficient amount for Beena without problem")
					QuestUpdate("Beanfield")
			}
			else if (findString("leave",choice1)){
				println("You decide to leave having done whatever business you wanted to do here")
				end = false
			}
			else{
				println("You"+ choice1 + ".......greeeat")
			}
			if (quest >= 3){
				QuestUpdate("Beanfield") //Check how to update Quests
			}
		}
	}
	//  CampName  = (width,length, tents, goblinChild,goblinWarrior,goblinArcher,goblinShaman)
	var goblinCamp1 = Array(30,30,4,6,4,0,0)
	var goblinCamp2 = Array(45,30,5,3,0,5,0)
	var goblinCamp3 = Array(40,40,3,7,2,4,1)
	var campArray = Array("goblinCamp1","goblinCamp2","goblinCamp3")
	 //Right now its a messy multi combat -> Could make combat Map
	 	//move speed/3 +1 if speed%3 !0 (so min 1 speed)
	 	//initiative means who goes first
	def Goblins()={
		println("You walk about searching for goblins and low and behold...you find a camp of goblins")
		println("what are you going to do?")
			//Add a Day/night function (?maybe use real time??)
		var camp = 0
		var campTents = campArray(camp)(2)
		var goblinChild = campArray(camp)(3)
		var goblinWarrior = campArray(camp)(4)
		var goblinArcher = campArray(camp)(5)
		var goblinShaman = campArray(camp)(6)
		var i = 0
		var enemyList = new Array[String](10)
		var numberOfEnemies = new Array[Int](10)
		if (goblinChild > 0){
			enemyList(i) = "goblinChild"
			numberOfEnemies(i) = goblinChild
			i+=1
		}
		if (goblinWarrior > 0){
			enemyList(i) = "goblinWarrior"
			numberOfEnemies(i) = goblinWarrior
			i+=1
		}
		if (goblinArcher > 0){
			enemyList(i) = "goblinArcher"
			numberOfEnemies(i) = goblinArcher
			i+=1
		}
		if (goblinShaman > 0){
			enemyList(i) = "goblinShaman"
			numberOfEnemies(i) = goblinShaman
		}
		var end = true
		while (end){
			println("[???]")
			var choice1 = readLine().toLowerCase()
			if (choice1 == "esc"){
				playerActions()
			}
			if (findString("attack",choice1)||findString("fight",choice1)){
				println("You charge in...good luck")

				multiCombat(numberOfEnemies, enemyList)

			}
			else if (findString("sneak",choice1)){
				println(s"Sneaking up to the camp, there are $campTents Tents")
				println("Which one will you sneak up to?")
				var choice2 = readLine().toLowerCase()
				if (choice2 == "esc"){
					playerActions()
				}
				if (choice2 != ""){
					println(s"you sneak up to a tent and peeking inside you see a number of sleeping goblins")
					println("What will you do?")
					println("[Charge,Assassinate,Leave]")
					var choice3 = readLine().toLowerCase()
					if (choice3 == "esc"){
						playerActions()
					}
					if (findString("Assassinate",choice3)){
						println("You sneak into the tent and kill everything in there with ease")
						for (i <- 0 to enemyList.length){//reduces numbers from all
							var numberRoll = diceRoll()-(10-numberOfEnemies(i))-2 //Kills from 0 to number of enemies
						//Change diceRoll to take (1,6) <- 1d6? because right now it's at 2d6 for everything (2 to 12)
							if (numberRoll < 0){
								numberRoll = 0
							}
							numberOfEnemies(i) -= numberRoll //Reduces enemy numbers
						}

					}
				}
			}
			else if (findString("leave",choice1)||findString("run",choice1)){
				println("Figuring you vs goblins isn't a good idea right now, you decide to run away")
				end = false
			}
			else{
				println("you "+ choice1+ "...great")
			}
		}
	}













	def Minefield()={
		println("Following Fergo's grunted intstructions, you finally come upone the field of veins, ready to be mined")
		println("What do you do?")
		var end = true
		while (end){
			println("[Mine,Explore,Leave]")
			var choice1 = readLine().toLowerCase()
			if (choice1 == "esc"){
				playerActions()
			}
			if (findString("mine",choice1)){
				//Create a 'Map' of the mines
				mineFieldMap()

			}
			else if (findString("explore",choice1)||findString("walk",choice1)){
				println("You walk around and see a bunch of stones with glowing crystals")

			}
			else{
				println("You've had enough of the mine-field. so you head back into the forest")
				end = false
			}
		}
	}
	def Bagel()={
		println("Walking through the Forest on the hunt for Bagel...you find him!")
		println("Its almost as if the universe wanted you to find him...")
		println("You find a what you guess is a human man, propped against a thick black tree, chest opened with innards splattered around the corpse.")
		println("His axe is embedded in that same tree, sap bleeding out in droplets")
		println("where do you want to go??")
		var end = true
		while(end){
			println("[Axe,Bagel,Leave]")
			var choice1 = readLine().toLowerCase()
			if (choice1 == "esc"){
				playerActions()
			}
			if (findString("axe",choice1)){
				println("You walk up to the axe embedded in the tree")
				println("what do you do?")
				var end2 = true
				while(end2){
					println("[Take,Leave]")
					var choice2 = readLine().toLowerCase()
					if (choice2 == "esc"){
						playerActions()
					}
					if (findString("take",choice2)){
						if (CS.Attributes.Might >= 3){
							println(s"You take out the axe, what a strong $CS.Attributes.Race you are")
							CS.Inventory.Iadd(1,"Bagel's Axe")
							end2 = false
						}
						else{
							println("You're a weakling, get someone stronger to do it")
						}

					}
					else if (findString("leave",choice2)){
						println("You decide not to touch the axe and instead walk away")
						end2 = false
					}
					else if (findString("attack",choice2)){
						Combat("Dryad",6,3,2,1)
						//Make a looting system based off combat??
					}
					else{
						println("You "+ choice2 +"...fantastic")
					}
				}

			}
			else if (findString("bagel",choice1)){
				println("You walk up to 'Bagel's' corpse (I mean you don't KNOW its him...but you suspect)")
				println("What do you do?")
				var end2 = true
				while(end2){
					println("[Inspect,Carry,Leave]")
					var choice2 = readLine().toLowerCase()
					if (choice2 == "esc"){
						playerActions()
					}
					if (findString("inspect",choice2)){
						println("He looks dead...the lack of innards might attribute to that fact")

					}
					else if (findString("carry",choice2)){
						println("You pick up Bagel...and stuff him in your magical backpack...ew")
						CS.Inventory.Iadd(1,"Bagel")
						end2 = false

					}
					else{
						println("You decide not to touch 'Bagel' and turn to leave")
						end2 = false
					}
				}
			}
			else if (findString("leave",choice1)){
				println("You decide its probably in your best interests to leave the murder scene alone and bugger off")
				end = false
			}
		}
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

	//Multi Combat
	def multiCombat(numberOfEnemies:Array[Int],enemyList:Array[String]):String={
		var combat = true
		while (combat){
			println("You have:"+CS.Attributes.Hits+" Hits")
			//Enemies get rolls depending on number of them
			for (i <- 0 to numberOfEnemies.length){//Gets length of the array
				
				var enemyQuery = s"Select Name, Hits, Attack, Defense FROM Enemies WHERE Name = '$enemyList[i]'" //MAKE THIS DB AT SOME POINT!!!
				var enemy = statement.executeQuery(enemyQuery)
				enemy.next()
				var enemyName = enemy.getString("Name")
				var enemyHits = enemy.getInt("Hits")
				var enemyAttack = enemy.getInt("Attack")
				var enemyDefense = enemy.getInt("Defense")

				var totalHits = enemyHits * numberOfEnemies(i)
				var damage = 0
				while (totalHits > 0){
					for (amount <- 0 to numberOfEnemies(i)){//Gets specific number of Enemies
						damage += attack(enemyName,enemyAttack,enemyDefense)
						totalHits -= damage //Need to Make Enemy DB and SQL the Attack and Defense and Hits of the Enemies
						while (damage >= enemyHits){
							damage -= enemyHits //Can kill multiple at once
							numberOfEnemies(i) -= 1 //Enemy is killed
						}
						if (CS.Attributes.Hits <= 0){
							combat = false
							println("You have died")
							death = false
							return "Lost"
						}
					}
				}
			}
		}
		return "Somehow you Won!"
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

	//Extra Random functions

	//


	//MAPS

	var Board= Array.ofDim[String](100,100)
	//
	def createGameBoard(width:Int,length:Int)={
		Board = Array.ofDim[String](width+1,length+1)
		for (A <- 0 to width){
			var B = 0
			for (B <- 0 to length){
				if (A == 0 && B == 0){
					Board(A)(B) = "   "
				}
				else if (A == 0){
					Board(A)(B) = (s" $B ")
					//print(Board(A)(B))
				}
				else if (B == 0){
					Board(A)(B) = (s" $A ")
				}
				else{
					Board(A)(B) = " ~ "
							//if (B <= length){
							//	print(Board(A)(B))
							//}
				}
			}
			print("\n")
		}
	}
	createGameBoard(6,5)
	def printGameBoard(Board:Array[Array[String]]):={
		var width = Board.length
		var length = Board(1).length
		for (A <-0 to width-1){
			for (B <- 0 to length-1){
				print(Board(A)(B))
			}
			print("\n")
		}
	}
	//

	//

	//Printing a tent on the board
	def tent(height,length):={
		var heightmax = Board.length-height
		var heightmin = 1 //0 is the numbers,
		var lengthmax = Board(1).length-length
		var lengthmin = 1 //1 is the edge of the board
		var randInt = scala.util.Random
		var randPositionHeight = randInt.nextInt(heightMax)+1 //1 to max
		var randPositionLength = randInt.nextInt(lengthMax)+1 //1 to max
		var row = 0
		for (A<- randPositionHeight to heightmax){
			row += 1
			var B = randPositionLength
			for (B<- randPositionHeight to lengthmax){
				row match{
					case 1 => //roof
						if (B >= randPositionLength+2 && B <= lengthmax){
							Board(A)(B) = "_"
						}
					case 2 => //midpoint
						if ( B == randPositionLength+1){
							Board(A)(B) = "/" //start of tent
						}
						else if (B == lengthmax-1){ //end of tent
							Board(A)(B) = """\"""
						}
					case 3 => //floor
						var m = 0
						var n = 0
						if (B == randPositionLength){
							Board(A)(B) = "/"
						}
						else if(B == lengthmax){
							Board(A)(B) = """\"""
						}
						else if (m < (lengthmax/3)){
							Board(A)(B) = "_"
							m+=1
						}
						else{
							n += 1
							Board(A)(B) = " "
							if(n >= m){
								m = 0
							}
						}
				}
			}
		}
	}
	//
	//
	def collisionDetection(height:Int,length:Int):Boolean={
		var heightmax = Board.length-height
		var heightmin = 1 //0 is the numbers,
		var lengthmax = Board(1).length-length
		var lengthmin = 1 //1 is the edge of the board
		for (A <- heightmin to heightmax){
			var B = lengthmin
			for (B <- lengthmin to lengthmax){
				if (Board(A)(B) != " - "){
					
				}
			}
		}
		//detect if tents placed in another tent area
	}

	//Right now its a messy multi combat -> Could make combat Map
	 	//move speed/3 +1 if speed%3 !0 (so min 1 speed)
	 	//initiative means who goes first
	 def goblinMap(goblinCamp:Array[Int]):={
	 	var width =goblinCamp(0)
	 	var length = goblinCamp(1)
	 	createGameBoard(width,length)
	 	var tents = goblinCamp(2) //number of tents
	 	//Create tents
	 	var randInt = scala.util.Random
	 	var tentType = randInt.nextInt(2)+1 //1 to 3
	 	tentType match{
	 		case 1 =>  //Round tent
	 			/*	 _____
					/	  \
			       /__   __\
	 			*/
	 			tent(3,9)
	 		case 2 => //Long tent
	 			/*	 ___________
					/		    \
				   /_____   _____\
	 			*/
	 			tent(3,15)//add horizontal/vertical later)
	 		//case 3 => //half tent
	 			/*	 __  __
					/	   \
				   /___     \
	 			*/
	 		//	tent(3,10)
	 	}
	 }




	//

	//
	def mineFieldMap()={
		var width = 7
		var length = 4
		createGameBoard(width,length)
		var randInt = scala.util.Random
		for (A <- 1 to width){
			var B = 1
			for (B <- 1 to length){
				var roll = randInt.nextInt(9)+1 //1 to 6
				roll match{
					case 1 => Board(A)(B) = " G " //green, harmless
					case 2|4|6 => Board(A)(B) = " - " //no mine
					case 3 => Board(A)(B) = " B " //blue, shatters()
					case 5 => Board(A)(B) = " R " //red, explosive()
					case 7 => Board(A)(B) = " Y " //yellow, harmless
					case 8 => Board(A)(B) = " N " //no crystals
					case 9 => Board(A)(B) = " P " //purple, harmless
				}
			}
		}
		var end = true
		while(end){
			println("Are you done mining?")
			println("[Yes,No]")
			//println("\n")
			var choice1 = readLine().toLowerCase()
			if (choice1 == "esc"){
				playerActions()
			}
			if (findString("y",choice1)){
				end = false
			}
			else{
				println("Key: G= Green, B= blue, R= red, Y= yellow, P = purple, N= no crystals, - = no mine")
				printGameBoard(Board)
				println("\n")
				println("Choose somewhere to Mine (x,y)")
				var mineChoice = readLine()
				var xCoord = coordGet(mineChoice.substring(0,2))
				var yCoord = coordGet(mineChoice.substring(2))
				var chosen = Board(yCoord)(xCoord)
				var roll2 = diceRoll()
				var rockCrab = false
				if (roll2 > 9){
					rockCrab = true
				}
				chosen match{
					case " G "|" P "|" Y " => println("You mine it and it drops a few crystals which quickly go colourless, alongside the chunks of ore you mine")
						if (rockCrab){
							Combat("Rock Crab",14,1,3,4)
							CS.Inventory.Iadd(2,"Crab Meat")
						}
						CS.Inventory.Iadd(3,"Iron Ore Chunk")
						//QuestUpdate("mineField") //check how this works
						CS.Inventory.Iadd(5,"White Crystals")
					case " N " => println("You mine it and some chunks of ore drop out for your efforts")
						if (rockCrab){
							Combat("Rock Crab",14,1,3,4)
							CS.Inventory.Iadd(2,"Crab Meat")
						}
						CS.Inventory.Iadd(3,"Iron Ore Chunk")
						//QuestUpdate("mineField")
					case " B " => println("it shatters upon the first blow")
						 CS.Attributes.Hits -= diceRoll()/2 //take 1d6 damage
						if (rockCrab){
							Combat("Rock Crab",8,1,3,1)
							CS.Inventory.Iadd(2,"Crab Meat")
						}
					case " R " => println("it explodes upon the first blow")
						CS.Attributes.Hits -= diceRoll() //2d6 damage
						if (rockCrab){
							Combat("Rock Crab",7,1,3,1)
							CS.Inventory.Iadd(2,"Crab Meat")
						}
					case " - " => println("you mine the empty ground...well done")
				}
				Board(yCoord)(xCoord) = " - "
			}
		}
	}

	def coordGet(number:String):Int={
		number match{
				case "1" | ",1" | "1," => 1
				case "2" | ",2" | "2," => 2
				case "3" | ",3" | "3," => 3
				case "4" | ",4" | "4," => 4
				case "5" | ",5" | "5," => 5
				case "6" | ",6" | "6," => 6
				case "7" | ",7" | "7," => 7
				case "8" | ",8" | "8," => 8
				case "9" | ",9" | "9," => 9
				case "10"| ",10"| "10,"=> 10
			}
	} 



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
