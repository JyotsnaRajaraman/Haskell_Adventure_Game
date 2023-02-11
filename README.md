# Functional Adventure Project

The game is set in a nightmare that you find yourself in being chased by a monster through an 'upside down' version of your house. In order to get out of it, you need to wake up or fight off the monster using a weapon you pick up from while navigating through the house. Here are a few rules:

1. The monster will catch up with you after ten moves and show up with a random object. Your object has to be better than the monster's in order to win. How will you know that it is better? No way to find out - the monster is stronger than you and may appear with an object that is not within your capacity to lift, since you can only lift objects that are lighter than you.

2. You have the ten moves to find something that you think will help you fight the monster, or wake up. To wake yourself up you need to find a needle and get to the bedroom! 

3. After the 10th move, your chosen weapon will be put to the test..but remember you can carry only one object at a time. So if you think something is better than the other - you may want to swap your current item out.

4. If you lose to the monster, your nightmare will reset until you win or exit

5. At any point in the game you can exit when you realise that this is just a dream

Instructions to win (for testing purposes)

```
-> take key 
```
```
-> unlock 
```
```
-> south
```
```
-> take needle
```
```
-> north
```
```
-> north
```
```
-> north
```
```
-> poke
```

## Commands
```
$ look -> will display where you are, objects you can see and directions you can move it
```
```
$ unlock -> requires you to have 'key' in your inventory
```
```
$ take [itemname] -> eg take key
```
```
$ drop [itemname] -> eg drop key 
```
```
$ [direction] -> will move you to the room in that direction, remember each such move counts towards total moves
```
```
$ steps -> will tell you how many moves you've made so far
```
```
$ inventory -> will display the object you are currently carrying
```
```
$ this is just a dream'/'exit'/'quit' -> will help you get out of the game
```
```
$ poke -> if you have a needle and are in the bedroom, you will wake yourself up and the nightmare will be over
```
## Project Requirements

### 3.1. Customize the Content: 

The game is now a nightmare set in a house, you start from the outside and have access to the following rooms: Hall, Attic, Basement, Bathroom, Bedroom. The items in the house are also customized for the game.

### 3.2. Feature of your choice:

1. New commands- poke and unlock and steps
2. Items have an additional component called 'value' which is used to pick the winner in the monster-human weapon battle
3. Moves are counted
4. After 10 moves, you must either win, or game will be reset
5. Monster is a counter move after 10 moves have been made that picks a random object from the house

### 3.3. Secret room

The house itself fulfils the 'secret room' which you must enter to finish the game. To do so you must pick up the key and use the command unlock to enter the house - there is no way to go back outside unless you lose to the monster. The key also no longer exists in your inventory after unlocking the front door.
