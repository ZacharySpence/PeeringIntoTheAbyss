def Welcome (name:String) = {
	println(s"Hello $name my friend");
}
def whoAreYou(): String = {
	println("Who are you?")
	return (readLine())
}

def taxing(salary:Int): Int = {
	var tax Double = 0.00
	if (salary  < 1000){
		return salary
	}
	else if (salary < 2000){
		tax = 0.15
	}
	else if (salary < 3000){
		tax = 0.17
	}
	else{
		tax = 0.21
	}
	println("Tax: " + tax*100 + "%")
	println(s"Salary: $salary")
	return (salary - (salary * tax)).toInt
}
println(taxing(2549))