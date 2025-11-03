import Pairing 

main :: IO ()
main = do
    putStrLn "=== Teste do Emparelhamento AVE ==="
    let t = TorneioAVE "Torneio Bilhar" ["A","B","C","D","E","F","G","H"] 3
    let r = ResultadosAVE [[("A","B",5,3),("C","D",2,5),("E","F",5,1),("G","H",4,5)]]
    print (runAVEParing t r)

    putStrLn "\n=== Teste do Emparelhamento de Eliminacao Direta ==="
    let t2 = TorneioElim "Torneio Andebol" ["A","B","C","D","E","F","G","H"]
    print (runElimParing t2)