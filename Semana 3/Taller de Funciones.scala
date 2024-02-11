def suc(n:Int) : Int = if (n>=0) n+1 else 0
def pred(n:Int) : Int = if(n>0) n-1 else 0

def pow(x:Int,n:Int) : Int = if(n==0) 1 else x*(pow(x,pred(n)))
def factorial(n:Int) : Int = if(n==0) 1 else n*(factorial(pred(n)))
def summatory(n:Int) : Int = if(n==0) n else n+(summatory(pred(n)))

def sum(x:Int,y:Int) : Int = if(x==0) y else suc(sum(pred(x),y))
def dif(x:Int,y:Int) : Int = if(x==y) 0 else suc(dif(x,suc(y)))
def mult(x:Int,y:Int) : Int = if(x==0) 0 else if(x==1) y else sum(y,mult(pred(x),y))
def div(x:Int,y:Int) : Int = if(x==0) 0 else if(y==0) 0 else suc(div(dif(x,y),y))
def mod(x:Int,y:Int) : Int = if(x<y) x else mod(dif(x,y),y)

pow(2,4) //Expected output: 16
factorial(5) //Exected output: 120
summatory(6) //Expected output: 21
sum(4,5) //Expected output: 9
dif(10,4) //Expected output: 6
mult(4,4) //Expected output: 16
div(40,2) //Expected output: 20
mod(5,2) //Expected output: 1