var A =2
for (A <- 1 to 20){
	var B = 1
	println(s"$A times multiplication table")
	for (B <- 1 to 20){
		print(s"$A x $B= " + A*B )
		if ((A*B)%2==0){
			println(" is Even")
		}
		else{
			println(" is Odd")
		}
	}
}
