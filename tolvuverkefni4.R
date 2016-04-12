#global breytur
medalgildi = 96
stadalfravik = 13
gildiN = c(5,9,16,28,50,90,160,280,500,
           900,1600,2800,5000,9000,16000)

#skilar n fjolda af vendigildum thar sem medalgildi er 96
#og stadalfravik er 13
vendiGildi = function(n){
  return(rnorm(n,mean = medalgildi, sd = stadalfravik))
}

#tekur vid vigri af tolum og skilar metli vigursins
reiknaMetilUrGildurm = function(gildi){
  medalgildiItrun = mean(gildi)
  result = 0

  for(i in 1:length(gildi)){
    result = result + ((gildi[i]-medalgildiItrun)^2)
  }

  return (sqrt(result/(length(gildi)-1)))

}

#tekur vid tolu n og skilar metili1 af n morgum venditolum
#med medalgildi 96 og stadalfraviki 13
metill1 = function(n){
  itrun = vendiGildi(n)
  medalgildiItrun = mean(itrun)
  result = 0

  for(i in 1:n){
    result = result + ((itrun[i]-medalgildiItrun)^2)
  }
  result = sqrt(1/(n-1) * result)

  return(result)
}

#tekur vid tolu n og skilar metili2 af n morgum venditolum
#med medalgildi 96 og stadalfraviki 13
metill2 = function(n){
  return( IQR( vendiGildi( n ) ) / 1.349 )
}

#skilar vigri med jafn morgum gildum og eru i gildiN.
#hvert stak i vigrinum inniheldur annad hvort dreifni
#metla1 eda bjogun theirra. Thad raedst af thvi hvort
#dreifni er FALSE eda TRUE
itraAllaMetlla1 = function (fjoldiItrana, dreifni){
  allirMetlar = c()

  for(i in 1:length(gildiN)){

    summaAllraItrana = c()

    for(k in 1:fjoldiItrana){
      summaAllraItrana = c(summaAllraItrana, metill1(gildiN[i]))
    }

    if(dreifni){
      summaAllraItrana = var(summaAllraItrana)
    }else{
      summaAllraItrana = mean(summaAllraItrana) - metill1(gildiN[i])
    }

    allirMetlar = c(allirMetlar,summaAllraItrana)
    print(i)
  }

  return(allirMetlar)
}

#skilar vigri med jafn morgum gildum og eru i gildiN.
#hvert stak i vigrinum inniheldur annad hvort dreifni
#metla2 eda bjogun theirra. Thad raedst af thvi hvort
#dreifni er FALSE eda TRUE
itraAllaMetlla2 = function(fjoldiItrana, dreifni){
    allirMetlar = c()

    for(i in 1:length(gildiN)){
      itranirAllaMetla = c()

      for(k in 1:fjoldiItrana){
          itranirAllaMetla = c( itranirAllaMetla, metill2(gildiN[i]))
      }

      if(dreifni){
        itranirAllaMetla = var(itranirAllaMetla)
      }else{
        itranirAllaMetla = mean(itranirAllaMetla) - metill2(gildiN[i])
      }

      allirMetlar = c(allirMetlar, itranirAllaMetla)
    }

    return(allirMetlar)
}

#skilar TRUE ef oryggisbilid inniheldur retta gildid
Oryggisbil = function(){

  x = vendiGildi(23)
  medaltal = mean(x)
  s = reiknaMetilUrGildurm(x)
  z1 = qnorm(.69)
  z2 = qnorm(.025)

  check = medalgildi + z1 * stadalfravik

  midja = medaltal + z1 * s
  bil = z2*(sqrt(s^2+(0.5*z1^2*s^2))/sqrt(23))

  if(check > midja + bil && check < midja - bil){
    return (TRUE)
  }
  return (FALSE)

}

################ daemi1 er a) lidur, daemi2 er b) lidur o.s.fr. ################

daemi1 = function (fjoldiItrana){
  yAs = itraAllaMetlla1(fjoldiItrana, FALSE)
  xAs = gildiN

  title = 'bjagi metils'
  pdf('mynd1.pdf')
  plot(x = log(xAs), y = yAs, type ='l', main = title)
}

daemi2 = function (fjoldiItrana){
  yAs = itraAllaMetlla1(fjoldiItrana, TRUE)
  xAs = gildiN

  title = 'dreifni metils'
  pdf('mynd2.pdf')
  plot(x = log(xAs), y = log(yAs), type ='l', main = title)
}

daemi3 = function(fjoldiItrana){
  yAs = itraAllaMetlla1(fjoldiItrana, TRUE) +
        itraAllaMetlla1(fjoldiItrana, FALSE)^2
  xAs = gildiN

  pdf('mynd3.pdf')
  plot(x = log(xAs), y = log(yAs), type = 'l')
}

daemi4 = function(fjoldiItrana){
  yAs = itraAllaMetlla2(fjoldiItrana, FALSE)
  xAs = gildiN

  pdf('mynd4.pdf')
  plot(x=log(xAs),y=yAs,type = 'l')
}

daemi5 = function(fjoldiItrana){
  yAs = itraAllaMetlla2(fjoldiItrana, TRUE)
  xAs = gildiN

  pdf('mynd5.pdf')
  plot(x=log(xAs),y=log(yAs),type='l')
}

daemi6 = function(fjoldiItrana){
  yAs = itraAllaMetlla2(fjoldiItrana, TRUE) +
        itraAllaMetlla2(fjoldiItrana, FALSE)^2
  xAs = gildiN

  pdf('mynd6.pdf')
  plot(x = log(xAs), y=log(yAs),type='l')
}

daemi8 = function (fjoldiItrana){
  fjoldiSemPassar = 0
  for(i in 1:fjoldiItrana){
    if(Oryggisbil()){
      fjoldiSemPassar = fjoldiSemPassar + 1
    }
  }
  return (fjoldiSemPassar/fjoldiItrana)
}
