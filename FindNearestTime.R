  
ProcessTimeVector <- function(T,t,tol) {

  
  #   % given a vector of time T- and another shorter vector t, find the indices of T, 
  #   closest to each element in t, within a tolerance, tol.
  #   %t=shorter array - R datenumbers
  #   %T=longer array, -R datenumbers
  #   %tol =tolerance
  #  use it on one element or an array
   #    This code can be used for two purposes
  # (a) call from witin a loop, and get all the values that fall within the stated range.
     # this allows gathering data, for temporal ggregation
  # (b) call a vector, and get the closest values that lie within that range. 
  #   %Gopal Mulukutla, November 2014
  #   
  
  
  # get the length of the shorter vector
  l=length(t);
    
  # condition 1, if you are looking process a single value.
  
  if (l==1) {
    
    chk=t;
    # find the elements of T, within the stated range.
    ind1=which(T >(chk-tol) & T <= (chk+tol));
  }
  
  else{
  
    # condition 2, if processing an array with more than one element.
    ind1= vector()
    
    for (i in 1:l ) {
      
      chk=t[i];
      # find all the values within the range....
      ind=which(T >(chk-tol) & T <= (chk+tol));
      
      
      len=length(ind)
      if(len==0){
        ind1[i]=NaN;i
      }
      else {
        #.... pick only one of the values. The nearest one
        
        ind1[i]=ind[1];
      }
      
      
      
    } 
    
    
  }
  
  
  
  return(ind1)
}

  
