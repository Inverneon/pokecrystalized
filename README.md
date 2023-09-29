# **Nayru's Pokedex Project: BETA PHASE**
- Branch: ***Nayrus-Pokedex-BETA***

*Bundled:* [Idain's](https://github.com/Idain) [Custom Dex Colors](https://github.com/pret/pokecrystal/wiki/Customizable-Pok%C3%A9dex-Color) and [Toggle Shiny Palettes](https://github.com/pret/pokecrystal/wiki/Option-to-show-shiny-colors-in-Pok%C3%A9dex), doesnt impact you if you dont want it.

*REQUIRED:* (bug/design flaw fix about Radio Tower)
- [Unique Mon Icons](https://github.com/pret/pokecrystal/wiki/Add-a-new-party-menu-icon) (or at least adapt to the new Bank structure, actual icons used are irrelevant)
- [Replace StatExp with EVs](https://github.com/pret/pokecrystal/wiki/Replace-stat-experience-with-EVs),
- [Expanded Tilesets to 255](https://github.com/pret/pokecrystal/wiki/Expand-tilesets-from-192-to-255-tiles), 
- my Gen3 Type/Status/Cat Tiles. Pokedex portion must be implemented. Other parts, optional.

FEATURES:  Base stats and info, Move Info, Detailed area info, Evoulation chart, and sprite page showing animated sprites (reflects shiny toggle too).

Move Information: Lvl-Up Moves -> Field Moves (plus lvl learned and/or TM/HM/Move Tutor) -> Egg Moves -> TMs. I have fully functional code commented out to include HMs. But I decided not to since Field Moves serve the same purpse. But, if you've messed with adding or deleting Field Moves, I reccomend scrapping the Field Moves code and restoring the HM code. Please feel free to DM about this if it's an issue. I'm working on better solutions at some point.
Move Page Future Ideas: Full Page toggle option, to show additional details about the moves, Type, power, category, accuracy, effects, etc

AREA Pages: Currently supports Johto/Kanto Walking/grass/surfing encounters plus their swarms. Fishing Rods information is also included. Right now, all it will show you is the relevant Fishing Group Name. In the near future, it will print Route information like the other pages.
To check your local Fishing Group, check your PokeGear.

POKEGEAR: Now shows the detailed name of map you're currently in (based on Map Group & Map Number) and it also reads the map's attribute to see it's fishing group, if it has one.

COMING SOON: AREA support for Headbutt Trees and Rock Smash Rocks and the Bug contest. Roaming Support for the dogs and any custom roamers.

I also have an idea to have a python script scrape all information about special event pokemon (like Celebi, Ho-oh, Lugia, Starters, Casinos, Gift pokemon like Eevee) and NPC trades and displaying that. The script should be run if you ever make edits to these and will recompile the list. I should probably do this for Field Moves too, since people can and do have custom Field Moves...

Feel Free to DM me any suggestions and feedback. If you want to help me design Tiles/GFX to help make everything easier to look at, I would be eternally grateful.

# Allow using a Field Move (HMs, Dig, Teleport, etc) if the Pokemon can learn it
- Branch: ***UseOverworld_Moves_IfCanLearn***
- Pokemon no longer need to have learned the Field Move in order to use it!!!
- Customizable Badge/TM/HM Requirements to allow for OPEN WORLD!!!
- [Tutorial](https://github.com/pret/pokecrystal/wiki/Allow-Using-a-Field-Move-if-the-Pokemon-Can-Learn-It)
![image](https://user-images.githubusercontent.com/110363717/189774794-2f15a7e5-150a-44e0-9704-bd35eb3ff2f8.png) ![image](https://user-images.githubusercontent.com/110363717/189774882-ce9c3f5b-17c2-4e3d-a4de-66a50cb6ed5b.png) *Major rewrite coming soon*

## Gen3-Style Type, Status, and Move Category Tiles, adapted from [Rangi's](https://github.com/Rangi42) [Polished Crystal](https://github.com/Rangi42/polishedcrystal)
- Branch: ***PC_Gen3_TypeStatusCategory_GFX***
- Tutorial: *Official publication coming soon*
- Co-authored and Supervised by [Idain](https://github.com/Idain)

## Kurt Makes Pokeballs Instantly, Adapated from [Rangi's](https://github.com/Rangi42) [Polished Crystal](https://github.com/Rangi42/polishedcrystal)
- Branch: ***PC_FastKurt***
- [Tutorial](https://github.com/pret/pokecrystal/wiki/Kurt-Makes-Pokeballs-Instantly)

## Harvest Multiple Berries / Apricorns from Berry Trees
- Branch: ***HarvestMultipleFruit***
- [Tutorial](https://github.com/pret/pokecrystal/wiki/Harvest-multiple-items-from-fruit-trees)

## Move Relearner & Egg Move Tutor/Relearner, originally adapted by [Idain](https://github.com/Idain) from [Rangi's](https://github.com/Rangi42) [Polished Crystal](https://github.com/Rangi42/polishedcrystal)
- The NPCs are located in Newbark Town, in Prof. Elm's Home!
- Branch: ***PC_Move_and_EggMove_Relearner***
- Tutorial: *coming soon?*

## **Coming Soon:** *Stats Screen:* Unown Font, Displaying DVs, StatExp or EVs, Hidden Power info, Hapiness

## **Coming Soon:** Show Caught Pokeball on Stats Screen, Adapted from [Rangi's](https://github.com/Rangi42) [Polished Crystal](https://github.com/Rangi42/polishedcrystal)

## **Coming Soon:** Bag Item Icons, Adapted from [Rangi's](https://github.com/Rangi42) [Polished Crystal](https://github.com/Rangi42/polishedcrystal)

# Pokémon Crystal [![Build Status][ci-badge]][ci]

This is a disassembly of Pokémon Crystal.

It builds the following ROMs:

- Pokemon - Crystal Version (UE) (V1.0) [C][!].gbc `sha1: f4cd194bdee0d04ca4eac29e09b8e4e9d818c133`
- Pokemon - Crystal Version (UE) (V1.1) [C][!].gbc `sha1: f2f52230b536214ef7c9924f483392993e226cfb`
- Pokemon - Crystal Version (A) [C][!].gbc `sha1: a0fc810f1d4e124434f7be2c989ab5b5892ddf36`
- CRYSTAL_ps3_010328d.bin `sha1: c60d57a24bbe8ecf7cba54ab3f90669f97bd330d`
- CRYSTAL_ps3_us_revise_010710d.bin `sha1: 391ae86b1d5a26db712ffe6c28bbf2a1f804c3c4`
- CGBBYTE1.784.patch `sha1: a25517f60ca0e887d39ec698aa56a0040532a4b3`

To set up the repository, see [INSTALL.md](INSTALL.md).

## See also

- [**FAQ**](FAQ.md)
- [**Documentation**][docs]
- [**Wiki**][wiki] (includes [tutorials][tutorials])
- [**Symbols**][symbols]

Other disassembly and/or decompilation projects:
* [**Pokémon Red and Blue**](https://github.com/pret/pokered)
* [**Pokémon Gold and Silver (Space World '97 demo)**](https://github.com/pret/pokegold-spaceworld)
* [**Pokémon Yellow**](https://github.com/pret/pokeyellow)
* [**Pokémon Trading Card Game**](https://github.com/pret/poketcg)
* [**Pokémon Pinball**](https://github.com/pret/pokepinball)
* [**Pokémon Stadium**](https://github.com/pret/pokestadium)
* [**Pokémon Gold and Silver**](https://github.com/pret/pokegold)
* [**Pokémon Ruby and Sapphire**](https://github.com/pret/pokeruby)
* [**Pokémon Pinball: Ruby & Sapphire**](https://github.com/pret/pokepinballrs)
* [**Pokémon FireRed and LeafGreen**](https://github.com/pret/pokefirered)
* [**Pokémon Emerald**](https://github.com/pret/pokeemerald)
* [**Pokémon Mystery Dungeon: Red Rescue Team**](https://github.com/pret/pmd-red)
* [**Pokémon Diamond and Pearl**](https://github.com/pret/pokediamond)
* [**Pokémon Platinum**](https://github.com/pret/pokeplatinum) 
* [**Pokémon HeartGold and SoulSilver**](https://github.com/pret/pokeheartgold)
* [**Pokémon Mystery Dungeon: Explorers of Sky**](https://github.com/pret/pmd-sky)

## Contacts

You can find us on:

* [Discord (PRET, #pokecrystal)](https://discord.gg/d5dubZ3)
* [IRC](https://web.libera.chat/?#pret)

[docs]: https://pret.github.io/pokecrystal/
[wiki]: https://github.com/pret/pokecrystal/wiki
[tutorials]: https://github.com/pret/pokecrystal/wiki/Tutorials
[symbols]: https://github.com/pret/pokecrystal/tree/symbols
[ci]: https://github.com/pret/pokecrystal/actions
[ci-badge]: https://github.com/pret/pokecrystal/actions/workflows/main.yml/badge.svg
