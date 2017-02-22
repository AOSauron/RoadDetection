object grey_level extends App {
   import com.tncy.top.image.ImageWrapper;


   def moyenne (x : Int) : Int = {
     //fait la moyenne en couleur d'un pixels
    var rouge:Int = ((x & 0x00FF0000)/math.pow(16,4)).toInt
    var vert:Int = ((x & 0x0000FF00)/math.pow(16,2)).toInt
    var bleu:Int = (x & 0x000000FF).toInt

    var moyenne: Int = ((rouge+vert+bleu)/3).toInt //on fait la moyenne et on arrondi
    return moyenne

  }


  def greylevel(table:Array[Array[Int]]):Array[Array[Int]]={
    //met au niveau de gris l'image table
   var pixelcourant : String = ""
   var moyenne_pixel : Int = 0
   var transparence0 : Int = 0
   var tableau_retour = table
    for (col <- 0 to wrappedImage.width-1 ) {
       for (row <-0 to wrappedImage.height-1 ) {
         transparence0 = ((tableau_retour(row)(col) & 0xFF000000)/math.pow(16,6)).toInt
         moyenne_pixel = moyenne(tableau_retour(row)(col))
         tableau_retour(row)(col) = (moyenne_pixel * (math.pow(16,4) + 16*16 + 1)).toInt + (math.pow(16,6)*transparence0).toInt
        }
     }
     return tableau_retour
  }

 def nagao(table:Array[Array[Int]]):Array[Array[Int]]={ //fait le floutage basique de l'image

    var tableau_retour : Array[Array[Int]] = table.map(_.clone)
    var minvariance : Int = 0
    var variance : Int = 0
    var pixel : List[Int] = Nil
    var moyenne : Int = 0
    var minmoyenne : Int = 0
    var detcol : Int = 0
    var detrow : Int =0
    var image2D_flou : Array[Array[Int]] = table
    image2D_flou = table;
     for (col <- 2 to wrappedImage.width-3) {
       for (row <-2 to wrappedImage.height-3) {

         //on calcule les coins
         //6 7 9
         //3 4 5
         //X 1 2
         detcol = -1
         detrow = -1
         for(i <- 0 to 4){
            pixel = Nil
            pixel = image2D_flou(row)(col) :: pixel
            pixel = image2D_flou(row)(col+detcol) :: pixel
            pixel = image2D_flou(row)(col+2*detcol) :: pixel
            pixel = image2D_flou(row+detrow)(col) :: pixel
            pixel = image2D_flou(row+detrow)(col+detcol) :: pixel
            pixel = image2D_flou(row+detrow)(col+2*detcol) :: pixel
            pixel = image2D_flou(row+2*detrow)(col) :: pixel
            pixel = image2D_flou(row+2*detrow)(col+detcol) :: pixel
            pixel = image2D_flou(row+2*detrow)(col+2*detcol) :: pixel
            moyenne = 0
            //on calcule la moyenne
            for(j<-0 to 8){
              moyenne = moyenne + (pixel(j) & 0x000000FF).toInt
             }
            moyenne = moyenne/9
            //on calcule la variance
            for(j<-0 to 8){
              variance = 0
              variance = variance + math.pow((pixel(j) & 0x000000FF)-moyenne,2).toInt
            }
            //on ignitialise le minimum de la variance
            minvariance =variance
            minmoyenne=moyenne
            if(detcol==1){detcol = -1;detrow+=1}
            else{detcol+=2}
          }

         //on calcule les cotes droits ver haut/bas
         //4  5  6  7  8
         //   1  2  3
         //      X
         detcol = -2
         for(k <- 0 to 1){
           pixel = Nil
           for(i<- math.min(0,detcol) to math.max(0,detcol)){
             for(j<- - math.abs(i) to math.abs(i)){
              pixel = image2D_flou(row+i)(col+j) :: pixel
              }
             }
             moyenne = 0
              //on calcule la moyenne
              for(j<- 0 to 8){
                moyenne = moyenne + (pixel(j) & 0x000000FF).toInt
               }
              moyenne = moyenne/9
              //on calcule la variance
              variance = 0
              for(j<-0 to 8){
                variance = variance + math.pow((pixel(j) & 0x000000FF)-moyenne,2).toInt
               }
              if(minvariance>variance){minvariance =variance ; minmoyenne=moyenne } //si v<=minV alors on retiens moyenne->minmoyenne
              detcol+=4
           }
         //on calcule les cotes droits gauche/droit
         //     4
         //  1  5
         //X 2  6
         //  3  7
         //     8
         detrow = -2
          for(k <- 0 to 1){
           pixel = Nil
           for(i<- math.min(0,detrow) to math.max(0,detrow)){
             for(j<- -math.abs(i) to math.abs(i)){
              pixel = image2D_flou(row+j)(col+i) :: pixel
              }
             }
             moyenne = 0
              //on calcule la moyenne
              for(j<-0 to 8){
                moyenne = moyenne + (pixel(j) & 0x000000FF).toInt
               }
              moyenne = moyenne/9
              //on calcule la variance
              variance = 0
              for(j<-0 to 8){
                variance = variance + math.pow((pixel(j) & 0x000000FF)-moyenne,2).toInt
               }
              if(minvariance>variance){minvariance =variance ; minmoyenne=moyenne }
              detrow += 4
           }
          //on calc le centre
         //1 2 3
         //4 x 5
         //6 7 8

            pixel = Nil
            pixel = image2D_flou(row)(col) :: pixel
            pixel = image2D_flou(row+1)(col) :: pixel
            pixel = image2D_flou(row+1)(col+1) :: pixel
            pixel = image2D_flou(row)(col+1) :: pixel
            pixel = image2D_flou(row-1)(col+1) :: pixel
            pixel = image2D_flou(row-1)(col) :: pixel
            pixel = image2D_flou(row-1)(col-1) :: pixel
            pixel = image2D_flou(row)(col-1) :: pixel
            pixel = image2D_flou(row+1)(col-1) :: pixel
            moyenne = 0
            for(j<-0 to 8){
              moyenne = moyenne + (pixel(j) & 0x000000FF).toInt
             }
            moyenne = moyenne/9
              //on calcule la variance
            variance = 0
            for(j<-0 to 8){
              variance = variance + math.pow((pixel(j) & 0x000000FF)-moyenne,2).toInt
             }
          if(minvariance>variance){minvariance =variance ; minmoyenne=moyenne }
          table(row)(col) = minmoyenne *(16*16*16*16 + 16*16 + 1).toInt +255*math.pow(16,6).toInt
          minvariance=0
      }
    }
  return tableau_retour
  }


  def gradient(table:Array[Array[Int]],seuil:Int):Array[Array[Int]]={
    //fait le gradient de l'image
    // 2  -1
    //-1
    var tableau_retour = table
    var seuil = 40
    var transparence0 : Int = 0
    var transparence1 : Int =0
    var transparence2 : Int = 0
    var pixel : Int = 0
    var image2D_gra : Array[Array[Int]] = Array(Array())
    image2D_gra = table;
     for (col <- 0 to wrappedImage.width-2 ) {
       for (row <-0 to wrappedImage.height-2 ) {//centre
          transparence0 = ((image2D_gra(row)(col) & 0xFF000000)/math.pow(16,6)).toInt
          transparence1 = ((image2D_gra(row+1)(col) & 0xFF000000)/math.pow(16,6)).toInt
          transparence2 = ((image2D_gra(row)(col+1) & 0xFF000000)/math.pow(16,6)).toInt
          pixel = math.abs((image2D_gra(row+1)(col) & 0x000000FF)*transparence1-(image2D_gra(row)(col) & 0x000000FF)*transparence0 + (image2D_gra(row)(col+1) & 0x000000FF)*transparence2-(image2D_gra(row)(col) & 0x000000FF)*transparence0).toInt

          if(pixel>=seuil){tableau_retour(row)(col)=0xFFFFFFFF;}//on test si la valeur est sup au seuil
          else{tableau_retour(row)(col)=0;}
         }
       }

   return tableau_retour
  }


 def contient(liste : List[Any], p : Any):Boolean={
   //return true  si p € liste
    var i:Int =0
    while(i<=liste.length-1){
      if(liste(i) == p){return true}
      i+=1
    }
   return false
  }

  def coloration_point(table:Array[Array[Int]],liste:List[List[Int]],couleur:Int):Array[Array[Int]]={
    //colorie les points de liste dans table
    var tableau_retour = table
    for(i<-0 to liste.length-1){
      tableau_retour(liste(i)(0))(liste(i)(1))=couleur
    }
    return tableau_retour
  }




  def check(table:Array[Array[Int]],colb:Int,rowb:Int,col:Int,row:Int,n:Int,chemin:List[List[Int]],ok:Boolean):List[List[Int]]={
    //fuction vérifiant si il existe un chemin de longueur n pas trop courbé
    // on doit initialiser point_mort = Nil avant toute utilisation !!
    var detcol = col - colb
    var detrow = row - rowb
    var nb = 0
    var tot :List[List[Int]]=Nil
    if(ok==false){ //cas terminal, on a pas de chemin
      point_mort = List(row,col) :: point_mort // on stock coordonées pixels dans la liste point_mort
      return Nil
      }
    else if (n==1){//cas terminal on a trouvé un chemin
      return List(List(row,col));
    }
    else if(detcol==0 || detrow == 0){// si on se déplace en ligne
      //4 2
      //X 1
      //5 3


      //1
      if(row+detrow>=0 && row+detrow<=table.length-1 && col+detcol>=0 && col+detcol<=table(0).length-1 &&  table(row+detrow)(col+detcol) == 0xFFFFFFFF && contient(chemin, List(row+detrow,col+detcol))==false && contient(point_mort, List(row+detrow,col+detcol))==false ){//on teste si on ne dépasse pas les bordures, si le pixel visé est blanc et n'est pas encore dans le chemin et si ce n'est pas un point mort
        tot = check(table,col,row,col+detcol,row+detrow,n-1,List(row+detrow,col+detcol)::chemin,true)//on cherche la fin du pixel en partant du pixel fixé
        if(tot!=Nil){nb+=1;  return (List(row,col) :: tot);}//si il y a un chemin on retourne le retour avec le pixel en plus
      }

      //2
      if(row+detcol+detrow>=0 && row+detcol+detrow<=table.length-1 && col+detcol+detrow>=0 && col+detcol+detrow<=table(0).length-1 && table(row+detcol+detrow)(col+detcol+detrow) == 0xFFFFFFFF && contient(chemin,List(row+detcol+detrow,col+detcol+detrow))==false  &&contient(point_mort,List(row+detcol+detrow,col+detcol+detrow))==false){
         var tot = check(table,col,row,col+detcol+detrow,row+detcol+detrow,n-1,List(row+detcol+detrow,col+detcol+detrow)::chemin,true)
         if(tot!= Nil){nb+=1;return (List(row,col) :: tot)}
      }

      //3
      if(row+detrow-detcol>=0 && row+detrow-detcol<=table.length-1 && col+detcol-detrow>=0 && col+detcol-detrow<=table(0).length-1 && table(row+detrow-detcol)(col+detcol-detrow) == 0xFFFFFFFF && contient(chemin,List(row+detrow-detcol,col+detcol-detrow))==false && contient(chemin,List(row+detrow-detcol,col+detcol-detrow))==false && contient(point_mort,List(row+detrow-detcol,col+detcol-detrow))==false){
        tot=check(table,col,row,col+detcol-detrow,row+detrow-detcol,n-1,List(row+detrow-detcol,col+detcol-detrow)::chemin,true)
        if(tot!=Nil){nb+=1;return (List(row,col) :: tot)}
      }

      //4
      if(row+detcol>=0 && row+detcol<=table.length-1 && col+detrow>=0 && col+detrow<=table(0).length-1 && table(row+detcol)(col+detrow) == 0xFFFFFFFF && contient(chemin,List(row+detcol,col+detrow))==false && contient(point_mort,List(row+detcol,col+detrow))==false){
        tot=check(table,col,row,col+detrow,row+detcol,n-1,List(row+detcol,col+detrow)::chemin,true)
        if(tot!=Nil){nb+=1;return (List(row,col) :: tot)}
      }

      //5
      if(row-detcol>=0 && row-detcol<=table.length-1 && col-detrow>=0 && col-detrow<=table(0).length-1 && table(row-detcol)(col-detrow) == 0xFFFFFFFF && contient(chemin,List(row-detcol,col-detrow))==false && contient(point_mort,List(row-detcol,col-detrow))==false){
         tot=check(table,col,row,col-detrow,row-detcol,n-1,List(row-detcol,col-detrow)::chemin,true)
        if(tot!=Nil){nb+=1;return (List(row,col) :: tot)}
      }

      // si ya rien on est sur dans un cul de sac
      if(nb==0){ check(table,colb,rowb,col,row,n-1,chemin,false)}
      else{ return Nil}//pour que scala accepte le code

    }
    else{// si on se déplace en diag
      if(row+detrow>=0 && row+detrow<=table.length-1 && col+detcol>=0 && col+detcol<=table(0).length-1 && table(row+detrow)(col+detcol) ==0xFFFFFFFF && contient(chemin,List(row+detrow,col+detcol))==false && contient(point_mort,List(row+detrow,col+detcol))==false){
        tot = check(table,col,row,col+detcol,row+detrow,n-1,List(row+detrow,col+detcol)::chemin,true)
        if(tot!= Nil){nb+=1;return( List(row,col) :: tot)}
      }
      if(row+detrow>=0 && row+detrow<=table.length-1 && col>=0 && col<=table(0).length-1 && table(row+detrow)(col)==0xFFFFFFFF && contient(chemin,List(row+detrow,col))==false && contient(point_mort,List(row+detrow,col))==false){
        tot = check(table,col,row,col,row+detrow,n-1,List(row+detrow,col)::chemin,true)
        if(tot!=Nil){nb+=1; return (List(row,col) :: tot)}
      }
      if(row>=0 && row<=table.length-1 && col+detcol>=0 && col+detcol<=table(0).length-1 && table(row)(col+detcol)==0xFFFFFFFF && contient(chemin,List(row,col+detcol))==false && contient(point_mort,List(row,col+detcol))==false){
        tot = check(table,col,row,col+detcol,row,n-1,List(row,col+detcol)::chemin,true)
        if(tot!=Nil){nb+=1; return (List(row,col) :: tot)}
      }
      if(row-detrow>=0 && row-detrow<=table.length-1 && col+detcol>=0 && col+detcol<=table(0).length-1 && table(row-detrow)(col+detcol)==0xFFFFFFFF && contient(chemin,List(row-detrow,col+detcol))==false && contient(point_mort,List(row-detrow,col+detcol))==false){
        nb+=1;
        return check(table,col,row,col+detcol,row-detrow,n-1,List(row-detrow,col+detcol)::chemin,true)
        }
      if(row+detrow>=0 && row+detrow<=table.length-1 && col+detcol>=0 && col+detcol<=table(0).length-1 && table(row+detrow)(col+detcol)==0xFFFFFFFF && contient(chemin,List(row,col+detcol))==false && contient(point_mort,List(row,col+detcol))==false){
        tot = check(table,col,row,col+detcol,row,n-1,List(row,col+detcol)::chemin,true)
        if(tot!=Nil){nb+=1; return (List(row,col) :: tot)}
      }
      if(nb==0){check(table,col,row,col,row,n-1,chemin,false)}
      else{return Nil}//pour que scala accepte le code
    }

  }


 def cercle(table:Array[Array[Int]],row:Int,col:Int,rayon:Int):List[List[Int]]={
   ///donne les coordonées des points contenu dans le disque de rayon #rayon et de centre (row,col)
    var retour : List[List[Int]] = Nil
    for(j<- -rayon to rayon){
      for (i<- -rayon+math.abs(j) to rayon-math.abs(j)){
          retour = List(row+i,col+j)::retour
       }
      }

    return retour
  }

  def debut_route_horizontal(table:Array[Array[Int]],row:Int,longeur_chemin:Int,rayon:Int,sens:Int):List[List[Int]]={
     //donne le début de la route
    var debut : List[List[Int]] = Nil // liste de retour
    var ch1 :List[List[Int]] = Nil   //chemin 1
    var ch2 :List[List[Int]] = Nil //chemin2
    var cercle2 : List[List[Int]] = Nil
    var j :Int =0
    var k2 :Int = 0
    var compteur : Int = 0
    var trouve = false
    var valide = false
    var rowb : Int  = row -sens
    for(i<-0 to table.length-2){//on parcours toute la colonne
      if(table(row)(i) == 0xFFFFFFFF && table(row)(i+1) == 0){//si on est dans un bors HAUT de route
        point_mort = Nil
        ch1 = check(table,i,rowb,i,row,longeur_chemin,Nil,true)//on prend un contour
        j=1
        trouve = false
        if(ch1!=Nil){//si le point est la base d'un chemin
           while(j<= rayon && i+j<=table(0).length-1 && trouve==false ){//on cherche un chemin à une distance #rayon
            var compt = i+j
            if(table(row)(i+j)==0xFFFFFFFF && table(row)(i+j-1)==0){//on élimine les contours sans noir avant
              point_mort = Nil
              ch2= check(table,i+j,rowb,i+j,row,longeur_chemin,Nil,true)
              if(ch2 !=Nil){ // si il y a un chemin
               compteur = 0
               for(k<- 0 to ch2.length-1){//on teste tout les points du parcours
                  cercle2 = cercle(table,ch2(k)(0),ch2(k)(1),rayon)
                  k2 = 0
                  valide = false
                  while(k2 <= cercle2.length-1 && valide == false){//on verifie que 60% des points de ch2 sont proches d'ua moins 1 point de ch1
                    if(contient(ch1,cercle2(k2))==true){
                      compteur +=1
                      valide = true
                    }
                    k2+=1
                  }
                }
                if(compteur>= 0.6*ch2.length){debut = List(List(row,i+1),List(row,i+j)) ::: debut}
                trouve = true
              }
            }
            j+=1
          }
        }
      }
    }
    return debut
  }


   def debut_route_vertical(table:Array[Array[Int]],col:Int,longeur_chemin:Int,rayon:Int,sens:Int):List[List[Int]]={
     //donne le début de la route
    var debut : List[List[Int]] = Nil // liste de retour
    var ch1 :List[List[Int]] = Nil   //chemin 1
    var ch2 :List[List[Int]] = Nil //chemin2
    var cercle2 : List[List[Int]] = Nil
    var j :Int =0
    var k2 :Int = 0
    var compteur : Int = 0
    var trouve = false
    var valide = false
    var colb : Int = col -sens
    for(i<-0 to table.length-2){
      if(table(i)(col) == 0xFFFFFFFF && table(i+1)(col) == 0){//si on est dans un bors GAUCHE de route
        point_mort = Nil
        ch1 = check(table,colb,i,col,i,longeur_chemin,Nil,true)//on prend un contour
        j=1
        trouve = false
         while(j<= rayon && i+j<=table.length-1 && trouve==false ){//on cherche un chemin à une distance #rayon
          var compt = i+j
          if(table(i+j)(col)==0xFFFFFFFF && table(i+j-1)(col)==0){//on élimine les contours sans noir avant
            point_mort = Nil
            ch2= check(table,colb,i+j,col,i+j,longeur_chemin,Nil,true)
            if(ch2 !=Nil){ // si il y a un chemin
             compteur = 0
             for(k<- 0 to ch2.length-1){
                cercle2 = cercle(table,ch2(k)(0),ch2(k)(1),rayon)
                k2 = 0
                valide = false
                while(k2 <= cercle2.length-1 && valide == false){
                  if(contient(ch1,cercle2(k2))==true){//on verifie que 60% des points de ch2 sont proches d'ua moins 1 point de ch1
                    compteur +=1
                    valide = true
                  }
                  k2+=1
                }
              }
              if(compteur>= 0.9*ch2.length){debut = List(List(i+1,col),List(i+j,col)) ::: debut}
              trouve = true
            }
          }
          j+=1
        }
      }
    }
    return debut
  }



  def coloration_vertical(table:Array[Array[Int]],colore:List[List[Int]],col:Int,sens:Int):List[List[List[Int]]]={
    ///fonction donnant les points à colorier dans la colone #col+1
    var colo : List[List[Int]] = Nil //liste de retour contenant les coordonées List(row,col) des points à colorier dans la colone col+1
    var dangereux : List[List[Int]] = Nil //liste de points suceptibles de poser problème
    var point_colo : List[Int] = Nil //liste des lignes des points coloriés dans la colone #col
    var k :Int = 0 // variable muete
    var blanc_avant : Boolean = false
    var blanc_apres : Boolean = false
    for(i <- 0 to table.length-1){//on detecte les points coloiriés de la colone précédente
      if(contient(colore,List(i,col))==true){
        point_colo = i :: point_colo
      }
    }
    for(i<-0 to point_colo.length-1){
        k=0
        blanc_avant = false
        blanc_apres = false
        if(table(point_colo(i))(col) == 0xFFFFFFFF){//test si il y a un blanc à coté
          blanc_avant = true
        }
        if(table(point_colo(i))(math.max(0,math.min(table.length-1,col+2*sens))) == 0xFFFFFFFF){//test si il y a un blanc de l'autre coté
          blanc_apres = true
        }
        /////vers le bas
      while(point_colo(i)+k>0 && point_colo(i)+k<table.length-1 && table(point_colo(i)+k)(col+sens)!=0xFFFFFFFF && (blanc_avant==false || blanc_apres==false)  && contient(colo,List(point_colo(i)+k,col+sens))==false && contient(dangereux,List(point_colo(i)+k,col+sens))==false){
        if(table(point_colo(i)+k+1)(col) == 0xFFFFFFFF){
          blanc_avant = true
        }
        if(table(point_colo(i)+k+1)(math.max(0,math.min(table.length-1,col+2*sens))) == 0xFFFFFFFF){
          blanc_apres = true
        }
        if( (blanc_apres == false || blanc_avant == false) && table(point_colo(i)+k+1)(col+sens)!=0xFFFFFFFF){
          colo = List(point_colo(i)+k,col+sens) :: colo
        }
        else{
          dangereux = List(point_colo(i)+k,col+sens) :: dangereux
        }
          k+=1
      }

      /////vers le haut
        blanc_avant = false
        blanc_apres = false

      if(table(point_colo(i))(col+sens)!=0xFFFFFFFF){
        k=1
        if(table(point_colo(i))(col)==0xFFFFFFFF || table(point_colo(i)-k)(col)==0xFFFFFFFF){
          blanc_avant = true
        }
        if(table(point_colo(i))(math.max(0,math.min(table.length-1,col+2*sens)))==0xFFFFFFFF || table(point_colo(i)-k)(math.max(0,math.min(table.length-1,col+2*sens)))==0xFFFFFFFF){
          blanc_apres = true
          }
        while(point_colo(i)-k>0 && point_colo(i)-k<table.length-1 && table(point_colo(i)-k)(col+sens)!=0xFFFFFFFF && (blanc_avant==false || blanc_apres==false)  && contient(colo,List(point_colo(i)-k,col+sens))==false && contient(dangereux,List(point_colo(i)-k,col+sens))==false){
        if(table(point_colo(i)-k-1)(col) == 0xFFFFFFFF){
            blanc_avant = true
          }
        if(table(point_colo(i)-k-1)(math.max(0,math.min(table.length-1,col+2*sens))) == 0xFFFFFFFF){
            blanc_apres = true
         }
       if((blanc_apres == false || blanc_avant == false) && table(point_colo(i)-k-1)(col+sens)!=0xFFFFFFFF ){
          colo = List(point_colo(i)-k,col+sens) :: colo
        }
        else{
          dangereux = List(point_colo(i)-k,col+sens) :: dangereux
        }

          k+=1
        }
       }

    }
    return List(colo,dangereux)
  }


   def coloration_horizontal(table:Array[Array[Int]],colore:List[List[Int]],row:Int,sens:Int):List[List[List[Int]]]={
    ///fonction donnant les points à colorier dans la colone #col+1
    var colo : List[List[Int]] = Nil //liste de retour contenant les coordonées List(row,col) des points à colorier dans la colone col+1
    var dangereux : List[List[Int]] = Nil //liste de points suceptibles de poser problème
    var point_colo : List[Int] = Nil //liste des lignes des points coloriés dans la colone #col
    var k :Int = 0 // variable muete
    var blanc_avant : Boolean = false
    var blanc_apres : Boolean = false
    for(i <- 0 to table.length-1){//on detecte les points coloiriés de la colone précédente
      if(contient(colore,List(row,i))==true){
        point_colo = i :: point_colo
      }
    }
    for(i<-0 to point_colo.length-1){
        k=0
        blanc_avant = false
        blanc_apres = false
        if(table(row)(point_colo(i)) == 0xFFFFFFFF){
          blanc_avant = true
        }
        if(table(math.max(0,math.min(table.length-1,row+2*sens)))(point_colo(i) )== 0xFFFFFFFF){
          blanc_apres = true
        }
        /////vers le bas
      while(point_colo(i)+k>0 && point_colo(i)+k<table(0).length-1 && table(row+sens)(point_colo(i)+k)!=0xFFFFFFFF && (blanc_avant==false || blanc_apres==false)  && contient(colo,List(row+sens,point_colo(i)+k))==false && contient(dangereux,List(row+sens,point_colo(i)+k))==false){
        if(table(row)(point_colo(i)+k+1) == 0xFFFFFFFF){
          blanc_avant = true
        }
        if(table(math.max(0,math.min(table.length-1,row+2*sens)))(point_colo(i)+k+1) == 0xFFFFFFFF){
          blanc_apres = true
        }
        if( (blanc_apres == false || blanc_avant == false) && table(row+sens)(point_colo(i)+k+1)!=0xFFFFFFFF){
          colo = List(row+sens,point_colo(i)+k) :: colo
        }
        else{
          dangereux = List(row+sens,point_colo(i)+k) :: dangereux
        }
          k+=1
      }

      /////vers le haut
        blanc_avant = false
        blanc_apres = false

      if(table(row+sens)(point_colo(i))!=0xFFFFFFFF){
        k=1
        if(table(row)(point_colo(i))==0xFFFFFFFF || (point_colo(i)-k>=0 && table(row)(point_colo(i)-k)==0xFFFFFFFF)){
          blanc_avant = true
        }
        if(table(math.max(0,math.min(table.length-1,row+2*sens)))(point_colo(i))==0xFFFFFFFF || (point_colo(i)-k>=0 && table(math.max(0,math.min(table.length-1,row+2*sens)))(point_colo(i)-k)==0xFFFFFFFF)){
          blanc_apres = true
          }
        while(point_colo(i)-k>0 && point_colo(i)-k<table(0).length-1 && table(row+sens)(point_colo(i)-k)!=0xFFFFFFFF && (blanc_avant==false || blanc_apres==false)  && contient(colo,List(row+sens,point_colo(i)-k))==false && contient(dangereux,List(row+sens,point_colo(i)-k))==false){
        if(table(row)(point_colo(i)-k-1) == 0xFFFFFFFF){
            blanc_avant = true
          }
        if(table(math.max(0,math.min(table.length-1,row+2*sens)))(point_colo(i)-k-1) == 0xFFFFFFFF){
            blanc_apres = true
         }
       if((blanc_apres == false || blanc_avant == false) && table(row+sens)(point_colo(i)-k-1)!=0xFFFFFFFF ){
          colo = List(row+sens,point_colo(i)-k) :: colo
        }
        else{
          dangereux = List(row+sens,point_colo(i)-k) :: dangereux
        }

          k+=1
        }
       }

    }
    return List(colo,dangereux)
  }


  def recherche_point(table:Array[Array[Int]]):List[List[List[Int]]]={
   var image_temp = gradient(nagao(greylevel(table)),30)
   var cherche : List[List[List[Int]]] = Nil
   var point_dangereux : List[List[Int]] = Nil
   var debut : List[List[Int]] = Nil // on cherche les points de début
   var point_colore_gauche : List[List[List[Int]]] = List(debut_route_vertical(image_temp,0,50,15,1))
   var point_colore_droite : List[List[List[Int]]] = Nil
   var point_colore_haut: List[List[List[Int]]] = List(debut_route_horizontal(image_temp,0,50,15,1))
   var point_colore_bas : List[List[List[Int]]] = Nil

   for(i<-0 to wrappedImage.width-4){ ////// on part dans ->
     cherche = coloration_vertical(image_temp,point_colore_gauche(i),i,1)
     point_colore_gauche = point_colore_gauche ::: List(cherche(0))
     point_dangereux = point_dangereux ::: cherche(1)
     println("(colone)on attaque : "+i,point_colore_gauche(i).length)
   }
   point_colore_droite = List(point_colore_gauche(point_colore_gauche.length-1) ::: debut_route_vertical(image_temp,wrappedImage.width-2,50,15,-1))
   for(i<-0 to wrappedImage.width-4){///// on repart dans <-
       var compteur = wrappedImage.width-2-i-1
       cherche = coloration_vertical(image_temp,point_colore_droite(0):::point_colore_gauche(compteur),wrappedImage.width-2-i,-1)
       point_colore_droite = cherche(0) :: point_colore_droite
       point_dangereux = point_dangereux ::: cherche(1)
       println("(colone)on attaque : "+compteur,point_colore_droite(0).length)
     }
   for(i<-0 to wrappedImage.height-4){ ////// on part dans de haut en bas
     cherche = coloration_horizontal(image_temp,point_colore_haut(i),i,1)
     point_colore_haut = point_colore_haut ::: List(cherche(0))
     point_dangereux = point_dangereux ::: cherche(1)
     println("(ligne)on attaque : "+i,point_colore_haut(i).length)
   }
    point_colore_bas = List(point_colore_haut(point_colore_haut.length-1) ::: debut_route_horizontal(image_temp,wrappedImage.height-2,50,15,-1))
   for(i<-0 to wrappedImage.height-4){///// on part de bas en haut
       var compteur = wrappedImage.height-2-i-1
       cherche = coloration_horizontal(image_temp,point_colore_bas(0):::point_colore_haut(compteur),wrappedImage.height-2-i,-1)
       point_colore_bas = cherche(0) :: point_colore_bas
       point_dangereux = point_dangereux ::: cherche(1)
       println("(ligne)on attaque : "+compteur,point_colore_bas(0).length)
     }
  return(point_dangereux::point_colore_gauche ::: point_colore_droite:::point_colore_haut:::point_colore_bas)
 }


   var fileName : String = "C:/Users/benoît/workspace/top_route/src/Images/ImagesTests/1.jpg";
   var wrappedImage : ImageWrapper = new ImageWrapper(fileName);
   var image2D : Array[Array[Int]] = wrappedImage.getImage();
   var image_temp : Array[Array[Int]] = Array(Array())
   var point_mort : List[List[Int]] = Nil
   var point = recherche_point(image2D)
   //var wrappedImage1 : ImageWrapper = new ImageWrapper(fileName);
  // var image2D_finale : Array[Array[Int]] = wrappedImage1.getImage();
   for(i <- 0 to point.length-1){ // on colorie
     image2D = coloration_point(image2D,point(i),0x000000FF)
   }
   //Fichier Destination (doit contenir l'extension du fichier).
   var outputFile1 : String = "C:/Users/benoît/workspace/top_route/src/Images/ImagesTests/final.png";
   // Sauvegarde le résultat.

   wrappedImage.saveImage(outputFile1);

   println("done")
}
