# Ustawiam ziarno dla powtarzalności wyników
set.seed(5)

# Zadanie 1
# Symuluję rzuty kostką dla 2, 10, 50, 100 i 1000 rzutów
rzuty <- c(2, 10, 50, 100, 1000)

# Przechodzę przez każdą liczbę rzutów
for (liczba_rzutow in rzuty) {
  
  # Symuluję rzuty kostką (zakres od 1 do 6)
  wyniki <- sample(x = 1:6, size = liczba_rzutow, replace = TRUE)
  
  # Sprawdzam, ile razy wypadły poszczególne liczby
  print(paste("Liczba rzutów:", liczba_rzutow))
  table(wyniki)
  
  # Obliczam średnią liczbę oczek
  srednia <- mean(wyniki)
  print(paste("Średnia liczba oczek:", srednia))
}

# Zadanie 2
# Inicjuję wektor, który będzie przechowywać wszystkie wyniki losowań
wszystkie_wyniki <- c()

# Powtarzam losowanie 1000 razy
for (i in 1:1000) {
  # Losuję 6 liczb z zakresu 1:49
  wyniki <- sample(x = 1:49, size = 6, replace = FALSE)
  
  # Dodaję wyniki do wektora z wszystkimi wynikami
  wszystkie_wyniki <- c(wszystkie_wyniki, wyniki)
}

# Używam funkcji table() do zliczenia, ile razy pojawiła się każda liczba
liczby_wystapien <- table(wszystkie_wyniki)
# Wyświetlam wyniki
liczby_wystapien

# Funkcja which.max() zwraca indeks największej wartości w wektorze. Potem wyciągam dzięki temu komórkę wektora która mnie interesuje
# Funkcja names() zwraca nazwy (indeksy) wektora liczby_wystapien. W tym przypadku będą to liczby od 1 do 49, które zostały wylosowane.
# Funkcja as.numeric() konwertuje wynik na typ numeryczny.
najczestsza_liczba <- as.numeric(names(liczby_wystapien)[which.max(liczby_wystapien)])
print(paste("Najczęściej pojawiająca się liczba:", najczestsza_liczba))

# Zadanie 3
# Ustalam prawdopodobieństwo wyrzucenia orła
p_orzel <- 0.55

# Definiuję wektor, który będzie przechowywał liczby rzutów
rzuty <- c(2, 3, 5, 20, 1000)

# Przechodzę przez każdą liczbę rzutów
for (liczba_rzutow in rzuty) {
  
  # Symuluję rzuty monetą, 1 to orzeł, 0 to reszka
  wyniki <- sample(c("Orzeł", "Reszka"), liczba_rzutow, TRUE, c(p_orzel, 1 - p_orzel))
  
  # Sprawdzam, ile razy wypadły orły i reszki
  liczba_orlow <- sum(wyniki == "Orzeł")
  liczba_reszek <- sum(wyniki == "Reszka")
  
  # Wyświetlam wyniki
  print(paste("Liczba rzutów:", liczba_rzutow))
  print(paste("Liczba orłów:", liczba_orlow))
  print(paste("Liczba reszek:", liczba_reszek))
  print("-----------------------------------")
}

# Zadanie 4
# Parametry
prob_knicks <- 0.65  # Prawdopodobieństwo wygrania meczu przez Knicks
prob_bulls <- 1 - prob_knicks  # Prawdopodobieństwo wygrania meczu przez Bulls
liczba_meczy <- 7  # Liczba meczów w serii
liczba_symulacji <- 100000  # Liczba symulacji Monte Carlo

# Inicjalizuję zmienną do zliczania, ile razy Bulls wygrają przynajmniej jeden mecz
licznik_bulls_wygrana <- 0

# Symulacja Monte Carlo z użyciem pętli for
for (i in 1:liczba_symulacji) {
  mecze <- sample(c("Knicks", "Bulls"),liczba_meczy, TRUE, c(prob_knicks, prob_bulls))
  if (any(mecze == "Bulls")) {  # Sprawdzam, czy Bulls wygrali przynajmniej jeden mecz
    licznik_bulls_wygrana <- licznik_bulls_wygrana + 1
  }
}

# Obliczam prawdopodobieństwo, że Bulls wygrają przynajmniej jeden mecz
prawdopodobienstwo_bulls_wygrana <- licznik_bulls_wygrana / liczba_symulacji
print(paste("Prawdopodobieństwo, że Bulls wygrają przynajmniej jeden mecz:", prawdopodobienstwo_bulls_wygrana))

# Zadanie 5
liczba_symulacji <- 100000  # Liczba symulacji Monte Carlo
wygrane_pistons <- 0  # Licznik wygranych serii przez Pistons

# Symulacja Monte Carlo
for (i in 1:liczba_symulacji) {
  # Pistons przegrywają pierwszy mecz
  mecz <- c("Clippers", "Pistons")
  
  # Inicjalizacja liczników wygranych meczów
  wygrane_clippers <- 1
  wygrane_pistons_mecze <- 0
  
  while (wygrane_clippers < 4 && wygrane_pistons_mecze < 4) {
    # Losuję, kto wygra następny mecz
    wynik_meczu <- sample(mecz, size = 1)
    
    if (wynik_meczu == "Pistons") {
      wygrane_pistons_mecze <- wygrane_pistons_mecze + 1
    } else {
      wygrane_clippers <- wygrane_clippers + 1
    }
  }
  
  # Sprawdzam, czy Pistons wygrali serię
  if (wygrane_pistons_mecze == 4) {
    wygrane_pistons <- wygrane_pistons + 1
  }
}

# Obliczam prawdopodobieństwo, że Pistons wygrają serię
prawdopodobienstwo_pistons_wygrana <- wygrane_pistons / liczba_symulacji
print(paste("Prawdopodobieństwo, że Pistons wygrają serię po przegraniu pierwszego meczu:", prawdopodobienstwo_pistons_wygrana))

# Zadanie 6
liczba_symulacji <- 100000  # Liczba symulacji Monte Carlo
wygrane_pistons <- 0  # Licznik wygranych serii przez Pistons

for (i in 1:liczba_symulacji) {
  mecz <- c("Clippers", "Pistons")
  
  # Pistons przegrywają pierwszy mecz
  wygrane_clippers <- 1
  wygrane_pistons_mecze <- 0
  
  while (wygrane_clippers < 4 && wygrane_pistons_mecze < 4) {
    wynik_meczu <- sample(mecz, 1, TRUE, c(0.55, 0.45))
    
    if (wynik_meczu == "Pistons") {
      wygrane_pistons_mecze <- wygrane_pistons_mecze + 1
    } else {
      wygrane_clippers <- wygrane_clippers + 1
    }
  }
  
  # Sprawdzam, czy Pistons wygrali serię
  if (wygrane_pistons_mecze == 4) {
    wygrane_pistons <- wygrane_pistons + 1
  }
}

# Obliczam prawdopodobieństwo, że Pistons wygrają serię
prawdopodobienstwo_pistons_wygrana <- wygrane_pistons / liczba_symulacji
print(paste("Prawdopodobieństwo, że Pistons wygrają serię po przegraniu pierwszego meczu:", prawdopodobienstwo_pistons_wygrana))

