medaltal = 96
stadalfravik = 13
gildiN = c(5,9,16,28,50,90,160,280,500,
           900,1600,2800,5000,9000,16000)

#Skilar melit... held eg...o-inu = s^1
#ur vendilbreytum af n breytistaerdum
#med videigandi medaltali og stadalfraviki.
MetillUrtaks = function(n){
  itrun = rnorm(n,mean = medaltal, sd = stadalfravik)
  medaltalItrun = mean(itrun)
  result = 0

  for(i in 1:n){
    result = result + ((itrun[i]-medaltalItrun)^2)
  }
  result = sqrt(1/(n-1) * result)

  return(result)
}

#fjoldiItrana er bara 10000 og dreifni aetti
#ad vera boolean breyta, TRUE ef vid erum ad reikna dreifni
#annars FALSE.
metlarFyrirOllGildi = function (fjoldiItrana, dreifni){
  allirMedalMetlar = c()

  for(i in 1:length(gildiN)){

    summaAllraItrana = c()

    for(k in 1:fjoldiItrana){
      summaAllraItrana = c(summaAllraItrana, MetillUrtaks(gildiN[i]))
    }
    if(dreifni){
      summaAllraItrana = var(summaAllraItrana)
    }else{
      summaAllraItrana = mean(summaAllraItrana)
      summaAllraItrana = summaAllraItrana - MetillUrtaks(gildiN[i])
    }
    allirMedalMetlar = c(allirMedalMetlar,summaAllraItrana)
    print(i)
  }
  return(allirMedalMetlar)
}

#i raun daemi 1 og 2. dreifni vaeri tha TRUE i daemi 2.
daemi1 = function (fjoldiItrana, dreifni){

  yAs = metlarFyrirOllGildi(fjoldiItrana, dreifni)
  xAs = gildiN

  if(dreifni){
    title = 'dreifni metils'
    pdf('mynd2.pdf')
  }else{
    title = 'bjagi metils'
    pdf('mynd1.pdf')
  }

  plot(x = log(xAs), y = log(yAs), type ='l', main = title)
  }


daemi1(10000, TRUE)
