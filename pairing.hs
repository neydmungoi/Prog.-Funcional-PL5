
-- pairing.hs
-- Módulo responsável pelo emparelhamento (Tarefa 2)

-- Funções principais:
--   runAVEParing  :: TorneioAVE -> ResultadosAVE -> ResultadosAVE
--   runElimParing :: TorneioElim -> ResultadosElim

module Pairing
(
  TorneioAVE(..),
  ResultadosAVE(..),
  TorneioElim(..),
  ResultadosElim(..),
  runAVEParing,
  runElimParing
) where

import Data.List (sortOn)

-- ============================================================
-- Tipos de dados
-- ============================================================

type Jogador = String
type AVE = Float
type Jogo = (Jogador, Jogador)
type Resultado = (Jogador, Jogador, Int, Int)

-- Estrutura de dados do torneio AVE
data TorneioAVE = TorneioAVE
  { nomeTorneio :: String,   -- nome do torneio
    jogadores :: [Jogador],  -- lista de jogadores
    rondas :: Int            -- número total de rondas
  } deriving (Show)

-- Estrutura de dados com resultados das rondas do sistema AVE
data ResultadosAVE = ResultadosAVE
  { resultadosAVE :: [[Resultado]]  -- lista de rondas, cada uma com os jogos
  } deriving (Show)

-- Estrutura de dados do torneio de eliminação direta
data TorneioElim = TorneioElim
  { nomeElim :: String,   -- nome do torneio
    equipas :: [Jogador]  -- lista de equipas participantes
  } deriving (Show)

-- Estrutura de dados com os emparelhamentos do sistema de eliminação
data ResultadosElim = ResultadosElim
  { jogosElim :: [[Jogo]]  -- lista de rondas com emparelhamentos
  } deriving (Show)

-- ============================================================
-- Funções auxiliares
-- ============================================================

-- | calcularAVE
-- Objetivo: calcular o valor médio (AVE) de cada jogador
-- Parâmetros:
--   [[Resultado]] -> lista de rondas com resultados de jogos
-- Retorna:
--   [(Jogador, AVE)] -> lista com o nome do jogador e o seu valor AVE
calcularAVE :: [[Resultado]] -> [(Jogador, AVE)]
calcularAVE rondas =
  let todosJogos = concat rondas
      -- acumula os frames ganhos e perdidos por jogador
      estatisticas =
        foldl
          (\acc (j1, j2, g1, g2) ->
              atualiza j1 g1 g2 (atualiza j2 g2 g1 acc))
          [] todosJogos
   in [(j, fromIntegral v / fromIntegral (v + d)) | (j, v, d) <- estatisticas]
  where
    -- Função auxiliar que atualiza as estatísticas de um jogador
    atualiza :: Jogador -> Int -> Int -> [(Jogador, Int, Int)] -> [(Jogador, Int, Int)]
    atualiza j g p [] = [(j, g, p)]
    atualiza j g p ((x, vg, vp):xs)
      | j == x = (x, vg + g, vp + p) : xs
      | otherwise = (x, vg, vp) : atualiza j g p xs

-- | emparelhar
-- Objetivo: criar pares de jogadores consecutivos numa lista
-- Parâmetros:
--   [Jogador] -> lista de jogadores ordenados
-- Retorna:
--   [Jogo] -> lista de pares (Jogador, Jogador)
emparelhar :: [Jogador] -> [Jogo]
emparelhar [] = []
emparelhar [x] = [] -- jogador sem par fica de fora
emparelhar (a:b:xs) = (a, b) : emparelhar xs

-- ============================================================
-- Emparelhamento pelo método AVE
-- ============================================================

-- | runAVEParing
-- Objetivo:
--   Criar uma nova ronda de emparelhamentos no sistema AVE,
--   com base nos valores médios (AVE) dos jogadores.
-- Parâmetros:
--   TorneioAVE -> informação do torneio
--   ResultadosAVE -> resultados existentes até ao momento
-- Retorna:
--   ResultadosAVE -> estrutura atualizada com uma nova ronda
runAVEParing :: TorneioAVE -> ResultadosAVE -> ResultadosAVE
runAVEParing torneio res =
  let aveJogadores = calcularAVE (resultadosAVE res)  -- calcular AVE
      -- ordenar por AVE (maior primeiro)
      ordenados = map fst (reverse (sortOn snd aveJogadores))
      -- emparelhar jogadores consecutivos
      novaRonda = emparelhar ordenados
      -- converter para estrutura de resultados (frames 0–0 até jogar)
      novosJogos = map (\(a,b) -> (a,b,0,0)) novaRonda
   in ResultadosAVE (resultadosAVE res ++ [novosJogos])

-- ============================================================
-- Funções auxiliares para Eliminação Direta
-- ============================================================

-- | emparelharElim
-- Objetivo: criar pares de equipas para uma ronda
-- Parâmetros: lista de equipas
-- Retorna: lista de emparelhamentos (Jogo)
emparelharElim :: [Jogador] -> [Jogo]
emparelharElim [] = []
emparelharElim [x] = [] -- número ímpar de equipas
emparelharElim (a:b:xs) = (a,b) : emparelharElim xs

-- | gerarRondasElim
-- Objetivo: gerar todas as rondas do torneio de eliminação direta
-- Parâmetros:
--   [Jogador] -> lista de equipas
-- Retorna:
--   [[Jogo]] -> lista de rondas (cada uma é uma lista de jogos)
gerarRondasElim :: [Jogador] -> [[Jogo]]
gerarRondasElim [] = []
gerarRondasElim [x] = [] -- vencedor encontrado, fim do torneio
gerarRondasElim xs =
  let pares = emparelharElim xs
      -- nesta fase, apenas cria a estrutura; os vencedores serão tratados depois
      vencedoresFicticios = map fst pares
   in pares : gerarRondasElim vencedoresFicticios

-- ============================================================
-- Emparelhamento pelo método de Eliminação Direta
-- ============================================================

-- | runElimParing
-- Objetivo:
--   Gerar todas as rondas do torneio de eliminação direta.
-- Parâmetros:
--   TorneioElim -> estrutura com o nome do torneio e as equipas
-- Retorna:
--   ResultadosElim -> estrutura com todas as rondas e emparelhamentos
runElimParing :: TorneioElim -> ResultadosElim
runElimParing torneio =
  let equipasT = equipas torneio
      rondas = gerarRondasElim equipasT
   in ResultadosElim rondas