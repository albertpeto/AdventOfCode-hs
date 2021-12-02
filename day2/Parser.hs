module Day2.Parser (Command(Up,Down,Forward), commands) where

import SimpleParser
import Control.Applicative

data Command = Up Int | Down Int | Forward Int

forward :: Parser Command
forward = Forward <$> (string "forward " >> number)

up :: Parser Command
up = Up <$> (string "up " >> number)

down :: Parser Command
down = Down <$> (string "down " >> number)

command :: Parser Command
command = forward <|> up <|> down

commands :: Parser [Command]
commands = many $ do {c <- command; newline; return c}