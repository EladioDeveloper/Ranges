class Range(var Start: Int, var End: Int) 
{ 
    def contains(array: Array[Int]): Boolean = {
        for(item <- array){
            if(item < Start || item > End)
            {
                return false;
            }
        }
          return true;
    }
    def notcontains(array: Array[Int]): Boolean = {
        return !contains(array);
    }
    def allPoints(): Array[Int] = {
        var difference = End - Start;
        var array:Array[Int] = new Array[Int](difference+1);
        for(i <- 0 to difference)
        {
            array(i) = Start + i;
        }
        return array;
    }
    def containsRange(range: Range): Boolean = {
        if(range.Start >= Start && range.End <= End) {
            return true;
        }
        else {
            return false;
        }
    }
    def notcontainsRange(range: Range): Boolean = {
        return !containsRange(range);
    }
    def endpoints(): Array[Int] = {
        return Array(Start, End);
    }
    def overLaps(range: Range): Boolean = {
        var difference = range.End - range.Start;
        var array = new Array[Int](difference + 1);
        for(i <- 0 to difference){
            array(i) = range.Start + i;
        }
        for(item <- array){
            if(item >= Start && item <= End){
                return true;
            }
        }
        return false;
    }

    def equals(range: Range): Boolean = {
        if(Start == range.Start && End == range.End){
            return true;
        }
        return false;
    }
    def noEquals(range: Range): Boolean = {
        return !equals((range));
    }
} 
  
object Practice_4  
{ 
    def main(args: Array[String])  { 
        
        var r1 = Validate("[2,6)");
        var r2 = Validate("(4,12]");
        var r3 = Validate("(-2,6)");
        var r4 = Validate("[-10,-3]");
    
        var rr1 = Validate("(1,4]");
        var rr2 = Validate("(5,11)");
        var rr3 = Validate("[-5,6)");
        var rr4 = Validate("(-10,0)");    

        var arr1 = Array(2,3,4,5);
        var arr2 = Array(5,6,7,8,9,10,11,12);
        var arr3 = Array(-1,0,1,2,3,4,8,20);
        var arr4 = Array(-10,-9,-8,-7,-6,-5,4,3);
        
        var a = 1;
        println("\nValidate");        
        println(a + ". " + Validate("[2,6)").endpoints().mkString(" "));a+=1; 
        println(a + ". " + Validate("(4,12]").endpoints().mkString(" "));a+=1; 
        println(a + ". " + Validate("{-2,6}"));a+=1; 
        println(a + ". " + Validate("[-10,0["));a+=1; 

        println("\nContains");
        println(a + ". " + r1.contains(arr1));a+=1;
        println(a + ". " + r2.contains(arr2));a+=1;
        println(a + ". " + r3.contains(arr3));a+=1;
        println(a + ". " + r4.contains(arr4));a+=1;
        
        println("\nNotcontains");
        println(a + ". " + r1.notcontains(arr1));a+=1;
        println(a + ". " + r2.notcontains(arr2));a+=1;
        println(a + ". " + r3.notcontains(arr3));a+=1;
        println(a + ". " + r4.notcontains(arr4));a+=1;
        
        println("\nAllPoints");
        println(a + ". " + r1.allPoints().mkString(" "));a+=1;
        println(a + ". " + r2.allPoints().mkString(" "));a+=1;
        println(a + ". " + r3.allPoints().mkString(" "));a+=1;
        println(a + ". " + r4.allPoints().mkString(" "));a+=1;
        
        println("\nContainsRange");
        println(a + ". " + r1.containsRange(rr1));a+=1;
        println(a + ". " + r2.containsRange(rr2));a+=1;
        println(a + ". " + r3.containsRange(rr3));a+=1;
        println(a + ". " + r4.containsRange(rr4));a+=1;
        
        println("\nNotContainsRange");
        println(a + ". " + r1.notcontainsRange(rr1));a+=1;
        println(a + ". " + r2.notcontainsRange(rr2));a+=1;
        println(a + ". " + r3.notcontainsRange(rr3));a+=1;
        println(a + ". " + r4.notcontainsRange(rr4));a+=1;
        
        println("\nEndPoints");
        println(a + ". " + r1.endpoints().mkString(" "));a+=1;
        println(a + ". " + r2.endpoints().mkString(" "));a+=1;
        println(a + ". " + r3.endpoints().mkString(" "));a+=1;
        println(a + ". " + r4.endpoints().mkString(" "));a+=1;
        
        println("\nOverLaps");
        println(a + ". " + r1.overLaps(Validate("[4,14]")));a+=1;
        println(a + ". " + r2.overLaps(Validate("(1,8)")));a+=1;
        println(a + ". " + r3.overLaps(Validate("(7,15)")));a+=1;
        println(a + ". " + r4.overLaps(Validate("(0,15)")));a+=1;
        
        println("\nEquals");
        println(a + ". " + r1.equals(Validate("(1,6)")));a+=1;
        println(a + ". " + r2.equals(Validate("[5,13)")));a+=1;
        println(a + ". " + r3.equals(Validate("(-2,0)")));a+=1;
        println(a + ". " + r4.equals(Validate("[-10,3]")));a+=1;
        
        println("\nNotEquals");
        println(a + ". " + r1.noEquals(Validate("(1,6)")));a+=1;
        println(a + ". " + r2.noEquals(Validate("[5,13)")));a+=1;
        println(a + ". " + r3.noEquals(Validate("(-2,0)")));a+=1;
        println(a + ". " + r4.noEquals(Validate("[-10,3]")));a+=1;
    } 
    
    def Validate(strRange: String): Range = {
        var cStart = strRange(0);
        var cEnd = strRange(strRange.length() - 1);
        if((cStart == '(' || cStart == '[') && (cEnd == ')' || cEnd == ']')){
            var clear = strRange.replaceAll("\\(", "").replaceAll("\\[", "").replaceAll("\\)", "").replaceAll("\\]", "").split(',');
            var start = clear(0).toInt;
            var end = clear(1).toInt;
            if(cStart == '(')
                start += 1;
            if(cEnd == ')')
                end -= 1;
            var range = new Range(start, end);
            return range;
        }
        else{
            return null;
        }
    }
} 