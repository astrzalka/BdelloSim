#' helpers
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

# funkcja licząca nową liczbę komórek gospodarza i bdello przy zadanej liczbie komórek potomnych - oblicza jedno pokolenie
oblicz_bdello <- function(n_gosp, n_bdello, n_potomnych){
  # nowa liczba komórek gospodarza
  n_gosp2 <- n_gosp - n_bdello
  # jeżeli liczba komórek gospodarza wyjdzie ujemna - przypisze 0
  if(n_gosp2 < 0){
    n_gosp2 <- 0
    # jeżeli bdello jest więcej niż gospodarzy zakładamy, że komórki potomne powstają tylko z tych które znajdą gospodarza, reszta przechodzi do kolejnego pokolenia
    n_bdello2 <- (n_bdello - n_gosp) + (n_gosp * n_potomnych)
  } else {
    # nowe bdello jeżeli jest ich mniej niż gospodarzy
    n_bdello2 <- n_bdello * n_potomnych
  }

  wynik <- c(n_gosp2, n_bdello2)
  return(wynik)
}
# testy
# oblicz_bdello(n_gosp = 1000, n_bdello = 10, n_potomnych = 4)
#
# oblicz_bdello(n_gosp = 1000, n_bdello = 10, n_potomnych = 2)
#
# oblicz_bdello(n_gosp = 790, n_bdello = 640, n_potomnych = 4)
#
# oblicz_bdello(n_gosp = 150, n_bdello = 2560, n_potomnych = 4)
#
# oblicz_bdello(n_gosp = 370, n_bdello = 640, n_potomnych = 2)

# funkcja oblicza zmianę komórek gospodarza i bdello aż liczba komórek gospodarza spadnie do 0
# dla jednego gospodarza
oblicz_czas_bdello <- function(n_gosp, n_bdello, n_potomnych, czas_bdello, plot = FALSE){

  # i zlicza pokolenia
  i = 1
  # liczba komórek gospodarza w aktualnym pokoleniu
  n_gosp_teraz <- n_gosp
  # ramka danych dla wyników, zawiera wartości startowe
  wyniki <- data.frame(n_gosp = n_gosp, n_bdello = n_bdello, czas = 0)

  # pętla dla kolejnych pokoleń aż skończą się komórki gospodarza
  while(n_gosp_teraz > 0){

    # oblicza nowe pokolenie
    wynik <- oblicz_bdello(wyniki[i,1], wyniki[i,2], n_potomnych)

    # dodaje czas
    nowy_wiersz <- c(wynik, czas_bdello * i)
    # dodaje pokolenie do wyników
    wyniki <- rbind(wyniki, nowy_wiersz)

    # nowe wartości pokolenia i liczby komórek gospodarza
    n_gosp_teraz <- wynik[1]
    i <- i + 1
  }

  # drukuje wynik - czas kiedy liczba komórek gospodarza spada do 0
  # print(paste('Czas do całkowitej lizy gospodarza wynosi', max(wyniki$czas)))

  # ewentualnie rysuje wykres zależności liczby komórek od czasu
  if(plot){

    p <- ggplot2::ggplot(wyniki)+
      ggplot2::geom_line(ggplot2::aes(x = czas, y = n_gosp), color = 'black')+
      ggplot2::geom_line(aes(x = czas, y = n_bdello), color = 'red')+
      ggplot2::theme_minimal()

    # print(p)
  }

  return(wyniki)
}
# testy
# oblicz_czas_bdello(n_gosp = 1000, n_bdello = 10, n_potomnych = 4, czas_bdello = 100)
#
# oblicz_czas_bdello(n_gosp = 1000, n_bdello = 10, n_potomnych = 2, czas_bdello = 100)
#
# oblicz_czas_bdello(n_gosp = 10000, n_bdello = 10, n_potomnych = 4, czas_bdello = 240, plot = TRUE)
# oblicz_czas_bdello(n_gosp = 10000, n_bdello = 10, n_potomnych = 2, czas_bdello = 180, plot = TRUE)
# oblicz_czas_bdello(n_gosp = 10000, n_bdello = 10, n_potomnych = 6, czas_bdello = 300, plot = TRUE)


# funkcja oblicza czas dla kilku bakterii na podstawie danych wpisanych do tabeli
# musi zawierać kolumny: gosp, n_gosp, n_bdello, n_potomnych, czas_bdello
oblicz_czas_bdello_wiele <- function(data_bdello, plot = TRUE){

  # pusta lista do wpisywania wyników
  wyniki_all <- list()

  # pętla dla każdego gospodarza osobno
  for(i in 1:nrow(data_bdello)){
    # wypisuje nazwę gospodarza
    # print(data_bdello$gosp[i])
    # oblicza wynik dla gospodarza
    wynik <- oblicz_czas_bdello(n_gosp = data_bdello$n_gosp_start[i],
                                n_bdello = data_bdello$n_bdello_start[i],
                                n_potomnych = data_bdello$n_potomnych[i],
                                czas_bdello = data_bdello$czas_bdello[i])
    # dodaje nazwę gospodarza do tabeli
    wynik$gospodarz <- data_bdello$gosp[i]

    wyniki_all[[i]] <- wynik
  }
  # zebranie wyników w tabelę
  wyniki_table <- do.call(rbind, wyniki_all)
  wyniki_table$n_gosp <- as.integer(wyniki_table$n_gosp)
  wyniki_table$n_bdello <- as.integer(wyniki_table$n_bdello)
  wyniki_table$czas <- as.integer(wyniki_table$czas)

  # opcjonalny wykres
  if(plot){

    p1 <- ggplot2::ggplot(wyniki_table, ggplot2::aes(x = czas, y = n_gosp, color = gospodarz))+
      ggplot2::geom_line()+
      ggplot2::theme_minimal()+
      ggplot2::ggtitle('Prey cell number')+
      ggplot2::ylab('n prey cells')+
      ggplot2::xlab('Time [min]')+
      ggplot2::scale_color_brewer(palette = 'Set1', name = 'Prey')

    p2 <- ggplot2::ggplot(wyniki_table, ggplot2::aes(x = czas, y = n_bdello, color = gospodarz))+
      ggplot2::geom_line()+
      ggplot2::theme_minimal()+
      ggplot2::ggtitle('Bdellovibrio cell number')+
      ggplot2::ylab('n Bdellovibrio cells')+
      ggplot2::xlab('Time [min]')+
      ggplot2::scale_color_brewer(palette = 'Set1', name = 'Prey')
    # wykresy obok siebie, legenda z boku, usuwa duplikaty legendy
    p <- p1 + p2 + plot_layout(guides = 'collect') &
      ggplot2::theme(legend.position='bottom')

    # wyświetla wykres
    # print(p)

  }

  # zwraca wyniki w formie tabeli
  return(list(p, wyniki_table))

}
bdello_optim <- function(n_bdello, n_gosp = 1000, n_potomnych = 4, czas_bdello = 240, czas_maks = 720){

  # liczba pokolen
  pok <- round(czas_maks/czas_bdello, 0)
  # liczba komórek gospodarza w aktualnym pokoleniu
  n_gosp_teraz <- n_gosp
  # ramka danych dla wyników, zawiera wartości startowe
  wyniki <- data.frame(n_gosp = n_gosp, n_bdello = n_bdello, czas = 0)

  # pętla dla kolejnych pokoleń aż skończą się komórki gospodarza
  for(i in 1:pok){

    # oblicza nowe pokolenie
    wynik <- oblicz_bdello(wyniki[i,1], wyniki[i,2], n_potomnych)

    # dodaje czas
    nowy_wiersz <- c(wynik, czas_bdello * i)
    # dodaje pokolenie do wyników
    wyniki <- rbind(wyniki, nowy_wiersz)

    # nowe wartości pokolenia i liczby komórek gospodarza
    n_gosp_teraz <- wynik[1]
    i <- i + 1
  }

  # drukuje wynik - czas kiedy liczba komórek gospodarza spada do 0
  #print(paste('Czas do całkowitej lizy gospodarza wynosi', max(wyniki$czas)))
  if(wyniki$n_gosp[pok + 1] == 0){
    wynik <- wyniki$n_gosp[pok + 1] + 1000000000
  } else {
    wynik <- wyniki$n_gosp[pok + 1]
  }

  return(wynik)
}


bdello_check <- function(n_bdello, n_gosp = 1000, n_potomnych = 4, czas_bdello = 240, czas_maks = 720){

  # liczba pokolen
  pok <- round(czas_maks/czas_bdello, 0)
  # liczba komórek gospodarza w aktualnym pokoleniu
  n_gosp_teraz <- n_gosp
  # ramka danych dla wyników, zawiera wartości startowe
  wyniki <- data.frame(n_gosp = n_gosp, n_bdello = n_bdello, czas = 0)

  # pętla dla kolejnych pokoleń aż skończą się komórki gospodarza
  for(i in 1:pok){

    # oblicza nowe pokolenie
    wynik <- oblicz_bdello(wyniki[i,1], wyniki[i,2], n_potomnych)

    # dodaje czas
    nowy_wiersz <- c(wynik, czas_bdello * i)
    # dodaje pokolenie do wyników
    wyniki <- rbind(wyniki, nowy_wiersz)

    # nowe wartości pokolenia i liczby komórek gospodarza
    n_gosp_teraz <- wynik[1]
    i <- i + 1
  }

  # wynik to końcowa liczba komórek gospodarza
  wynik <- wyniki$n_gosp[pok + 1]

  return(wynik)
}

ile_bdello <- function(n_gosp_o = 1000, n_potomnych_o = 4, czas_bdello_o = 240, czas_maks_o = 720){

  wynik <- optim(par = c(10), bdello_optim,# method = 'Brent', lower = 10, upper = 10000,
                 n_gosp = n_gosp_o, n_potomnych = n_potomnych_o, czas_bdello = czas_bdello_o, czas_maks = czas_maks_o)

  wynik <- round(wynik$par)
  # print('Początkowa liczba komórek Bdellovibrio')
  # sprawdz czy wynik jest ok, jak za mały to dodaj 1
  if(bdello_optim(n_bdello = wynik, n_gosp = n_gosp_o, n_potomnych = n_potomnych_o,
                  czas_bdello = czas_bdello_o, czas_maks = czas_maks_o) == 1000000000){
    return(wynik)
  } else {
    return(wynik + 1)
  }

}
