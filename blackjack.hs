
import System.IO
import System.Random
data Letters = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Joker | Queen | King
          deriving (Show, Enum, Read, Bounded)

--La funcion recibe StdGen que es el numero pseudoaleatorio de Random, así como un Int. Para retornar una lista de nuestro dato de las cartas
--randomR es el metodo de random para generar un numero entre un rango() usando un y un numero pseudoaleatorio gen entonces hacemos eso en 
--la linea del where igualamos randomNumber y newGen con un nuevo valor
drawDealerCards :: StdGen -> Int -> [Letters]
drawDealerCards gen total | total < 11 && randomNumber == 1 = toEnum (randomNumber - 1) : drawDealerCards newGen (total + randomNumber + 10)
                          | total < 17 = toEnum (randomNumber - 1) : drawDealerCards newGen (total + randomNumber)
                          | otherwise = []
                          where (randomNumber, newGen) = randomR (1,13) gen :: (Int, StdGen)

startJugadorCards :: StdGen -> Int -> [Letters]
startJugadorCards gen stop | stop /= 0 = toEnum (randomNumber - 1) : startPlayerCards newGen (stop - 1)
                          | otherwise = []
                          where (randomNumber, newGen) = randomR (1,13) gen :: (Int, StdGen)
--Retorna una carta de la lista, por eso en toEnum 
drawJugadorCard :: StdGen -> [Letters]
drawJugadorCard gen = [toEnum (randomNumber - 1)]
    where (randomNumber, newGen) = randomR (1,13) gen :: (Int, StdGen)

-- Display value of cards
handValue :: [Letters] -> [Int]
handValue [] = []
handValue (x:xs) | fromEnum x >= 10 = fromEnum 10 : handValue xs
                 | otherwise        = (fromEnum (x) + 1) : handValue xs

-- output of dealers cards to screen for user to see
showCards :: [Letters] -> String
showCards [] = ""
showCards (x:xs) | fromEnum x   < 10 && fromEnum x > 0 = show (fromEnum (x) + 1) ++ ", " ++ showCards xs
                 | otherwise = show x ++ ", " ++ showCards xs

-- Display cards enum
handDisplay :: [Letters] -> [Int]
handDisplay [] = []
handDisplay (x:xs) = (fromEnum (x) + 1) : handDisplay xs

--Display card of Value chosen
displayCard :: [Int] -> [Letters]
displayCard [] = []
displayCard (x:xs) = toEnum (x - 1) : displayCard xs

-- Sum cards
sumCards :: [Int] -> Int
sumCards letters | sum letters < 12 && elem 1 letters = sum (letters) + 10
                 | otherwise = sum letters

-- Show crupier and player points and cards
pointTotal :: [Letters] -> [Letters] -> String
pointTotal dealerCards playerCards | sumCards (handValue dealerCards) == sumCards (handValue playerCards) = "Game Over"
                                   | sumCards (handValue dealerCards) > sumCards (handValue playerCards) = "You Lose"
                                   | sumCards (handValue dealerCards) < sumCards (handValue playerCards) = "You Win"

{-
pointTotal' :: ([Int] -> Int) -> [Letters] -> [Letters] -> String
pointTotal f dealerCards playerCards | f (handValue dealerCards) == f (handValue playerCards) = "Game Over"
                                     | f (handValue dealerCards) > f (handValue playerCards) = "You Lose"
                                     | f (handValue dealerCards) < f (handValue playerCards) = "You Win"
-}

-- Rules
{-rules :: IO ()
rules = do
    run-}

blackjack :: IO ()
blackjack = run

run = do
    newStdGen
    putStrLn "\nREGLAS"
    putStrLn "1.- Tu objetivo es alcanzar o tener un aproximado de 21 puntos."
    putStrLn "2.- Si el crupier tiene un puntaje mas alto, pierdes"
    putStrLn "3.- Si ganas, obtendras el doble de lo que apostaste"
    putStrLn "4.- Empiezas con la mitad del dinero a ganar"
    putStrLn "5.- Si te quedas sin 'dinero' pierdes, si alcanzas el puntaje dado, ganas!"
    putStrLn "\nBLACKJACK\n1.-Jugar\n2.-Salir\n"
    ans <- getLine
    if (read ans::Int) == 1 then do
        playBlackJack
    else if (read ans::Int) == 2 then
        putStrLn "Adios"
    else do
        putStrLn "Opcion invalida"
        run

-- Game
playBlackJack :: IO ()
playBlackJack = 
    do
        newStdGen 
        g <- getStdGen
        putStrLn "Cantidad de dinero a juntar para ganar: "
        balance <- getLine
        if (read balance::Double) < 19 then do
            putStrLn "Valor invalido!"
            playBlackJack
        else do
            playerGen <- getStdGen
            dealerGen <- newStdGen
            game (drawDealerCards dealerGen 0) (startJugadorCards playerGen 2) ((read balance::Double) / 2::Double) (read balance::Double) 0
            
game :: [Letters] -> [Letters] -> Double -> Double -> Double -> IO () 
game dealerCards playerCards curBalance tarBalance bet =
    do
        newStdGen
        if curBalance > tarBalance then do
            putStrLn "Superaste los puntos!\nQuieres jugar de nuevo? y/n: "
            command <- getLine
            if command == "y" then do
                putStrLn "Ok!"
                run
            else 
                putStrLn "Goodbye!"
        else if bet == 0 then do
            putStrLn $ "Dinero actual: " ++ show curBalance ++ " / dinero a superar: " ++ show tarBalance
            putStrLn "Ingresa la cantidad de dinero a apostar!"
            betValue <- getLine
            if (read betValue::Double) < 1 && (read betValue::Double) > curBalance then do
                putStrLn "Apuesta invalida!!"
                game dealerCards playerCards curBalance tarBalance 0
            else do
                putStrLn "Apuesta Aceptada!"
                game dealerCards playerCards curBalance tarBalance (read betValue::Double)
        else if sumCards  (handValue dealerCards) > 21 then do
            putStrLn $ "Dealer cards: " ++ showCards dealerCards
            putStrLn $ "Jugador cards: " ++ showCards playerCards
            putStrLn "El crupier perdio!, ganas!"
            dealerGen <- getStdGen
            playerGen <- newStdGen
            game (drawDealerCards dealerGen 0) (startJugadorCards playerGen 2) (curBalance + bet) tarBalance 0
        else if  sumCards (handValue playerCards) > 21 then do
            putStrLn $ "Dealer cards: " ++ showCards dealerCards
            putStrLn $ "Jugador cards: " ++ showCards playerCards
            putStrLn "Perdiste!!"
            if curBalance - bet < 1 then do
                putStrLn "Perdiste todo tu dinero!"
                run
            else do
                dealerGen <- getStdGen
                playerGen <- newStdGen
                game (drawDealerCards dealerGen 0) (startJugadorCards playerGen 2) (curBalance - bet) tarBalance 0
        else if sumCards (handValue playerCards) == 21 then do
            putStrLn $ "Crupier cards: " ++ showCards dealerCards
            putStrLn $ "Jugador cards: " ++ showCards playerCards
            putStrLn "BLACKJACK!!, ganaste 1.5 puntos mas!"
            dealerGen <- getStdGen
            playerGen <- newStdGen
            game (drawDealerCards dealerGen 0) (startJugadorCards playerGen 2) (curBalance + (bet/2) + bet) tarBalance 0
        else if sumCards (handValue playerCards) == sumCards (handValue dealerCards) then do
            putStrLn $ "Crupier cards: " ++ showCards dealerCards
            putStrLn $ "Jugador cards: " ++ showCards playerCards
            putStrLn "Fue un empate!, no pierdes nada."
            playerGen <- getStdGen
            dealerGen <- newStdGen
            game (drawDealerCards dealerGen 0) (startJugadorCards playerGen 2) curBalance tarBalance 0
        else do
            putStrLn $ "Crupier cards: " ++ showCards dealerCards
            putStrLn $ "Jugador cards: " ++ showCards playerCards
            putStrLn "Tomar carta o Terminar? hit/stand"
            command <- getLine
            if command == "hit" then do
                playerGen <- getStdGen
                let newCard = drawJugadorCard playerGen
                let newJugadorCards = playerCards ++ newCard
                game dealerCards newJugadorCards curBalance tarBalance bet
            else if command == "stand" then
                if  pointTotal dealerCards playerCards == "You Win" then do                    
                    putStrLn $ "Crupier cards: " ++ showCards dealerCards
                    putStrLn $ "Jugador cards: " ++ showCards playerCards
                    putStrLn "Ganaste!, tu puntaje fue mas alto"
                    playerGen <- getStdGen
                    dealerGen <- newStdGen
                    game (drawDealerCards dealerGen 0) (startJugadorCards playerGen 2) (curBalance + bet) tarBalance 0
                else if pointTotal dealerCards playerCards == "You Lose" then do
                    putStrLn $ "Crupier cards: " ++ showCards dealerCards
                    putStrLn $ "Jugador cards: " ++ showCards playerCards
                    putStrLn "Perdiste, puntaje del dealer fue mayor"
                    if curBalance - bet < 1 then do
                        putStrLn "Perdiste todo tu dinero!"
                        run
                    else do
                        dealerGen <- getStdGen
                        playerGen <- newStdGen
                        game (drawDealerCards dealerGen 0) (startJugadorCards playerGen 2) (curBalance - bet) tarBalance 0
                else do 
                    putStrLn $ "Crupier cards: " ++ showCards dealerCards
                    putStrLn $ "Jugador cards: " ++ showCards playerCards
                    putStrLn "Fue un empate!, no pierdes nada."
                    playerGen <- getStdGen
                    dealerGen <- newStdGen
                    game (drawDealerCards dealerGen 0) (startJugadorCards playerGen 2) curBalance tarBalance 0
            else do
                putStrLn "Opcion Incorrecta, ingresa hit o stand"
                game dealerCards playerCards curBalance tarBalance bet
        
