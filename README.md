# Dark Castle

**Dungeon crawler on isometric perspective**

![plot](./screenshots/map_directory_2.jpg)

This is a dungeon crawler game with an isometric perspective, featuring light sources and shadows. The game includes a level editor, allowing players to design and create various scenes within the game itself.

Here are the features supported by the game engine:

- 3D rendering of textures and sprites with isometric perspective.
- The scene is divided into squares, with each square having defined floor and ceiling heights. This allows for lower, mid, and upper textures for each square, plus an additional texture for the floor, similar to the old Doom game engine.
- Up to nine different light sources can be defined in each scene, along with ambient light.
Shadows are depicted for static elements in the scene. However, the shadow algorithm only calculates shadows for static elements; characters and moving objects do not project shadows.
- Rendering of sprites for objects, which can be static or animated (if the object has multiple sprites).
- Rendering of playable characters that can navigate through the scenes.
- Doors that can be opened and closed, connecting different scenes.

The rendering of scenes is done without the aid of 3D graphics libraries (OpenGL or similar); everything is implemented and calculated pixel by pixel. This is why the shadow algorithm is not dynamic and cannot render moving objects, as the calculation of shadows is computationally intensive and cannot be done in real-time (it is pre-calculated and used when the scene is rendered).

All the textures seen in the screenshots come from Doom, Doom II, Heretic, and Hexen computer games. These games are from the 90s, but their graphical resources are not free, so I cannot upload them to this repository. Also, the background image on the menu screens comes from DevianArt website, but I do not know who is the author and I cannot include credits for it.

The game is developed using FreeBASIC (version 0.18) and is not fully finished. There are not any enemies or interactive objects; only navigation through the scenes is fully functional.

## Screenshots

Main menu:

![menu_main](./screenshots/menu_main.jpg)

Choose entry point on he adventure:

![menu_entry_point](./screenshots/menu_entry_point.jpg)

Options menu:

![menu_options](./screenshots/menu_options.jpg)

Credits screen:

![menu_credits](./screenshots/menu_credits.jpg)

Directory 1:

![map_directory_1](./screenshots/map_directory_1.jpg)

Directory 2:

![map_directory_2](./screenshots/map_directory_2.jpg)

Cementery map 1:

![map_cementery_1](./screenshots/map_cementery_1.jpg)

Cementery map 2:

![map_cementery_2](./screenshots/map_cementery_2.jpg)

Corridor map 1:

![map_corridor_1](./screenshots/map_corridor_1.jpg)

Corridor map 2:

![map_corridor_2](./screenshots/map_corridor_2.jpg)

Downstairs map 1:

![map_downstairs_1](./screenshots/map_downstairs_1.jpg)

Downstairs map 2:

![map_downstairs_2](./screenshots/map_downstairs_2.jpg)

Entrance map 1:

![map_entrance_1](./screenshots/map_entrance_1.jpg)

Entrance map 2:

![map_entrance_2](./screenshots/map_entrance_2.jpg)

Entrance map 3:

![map_entrance_3](./screenshots/map_entrance_3.jpg)

Entrance map 4:

![map_entrance_4](./screenshots/map_entrance_4.jpg)

Laboratory map 1:

![map_laboratory_1](./screenshots/map_laboratory_1.jpg)

Laboratory map 2:

![map_laboratory_2](./screenshots/map_laboratory_2.jpg)

Laboratory map 3:

![map_laboratory_3](./screenshots/map_laboratory_3.jpg)

Laboratory map 4:

![map_laboratory_4](./screenshots/map_laboratory_4.jpg)

Laboratory map 5:

![map_laboratory_5](./screenshots/map_laboratory_5.jpg)

Library map 1:

![map_library_1](./screenshots/map_library_1.jpg)

Library map 2:

![map_library_2](./screenshots/map_library_2.jpg)

Portal map 1:

![map_portal_1](./screenshots/map_portal_1.jpg)

Portal map 2:

![map_portal_2](./screenshots/map_portal_2.jpg)

Tavern map 1:

![map_tavern_1](./screenshots/map_tavern_1.jpg)

Tavern map 2:

![map_tavern_2](./screenshots/map_tavern_2.jpg)

Tavern map 3:

![map_tavern_3](./screenshots/map_tavern_3.jpg)

Shadow testing 1:

![map_shadow](./screenshots/map_shadow_test_1.jpg)

Shadow testing 2:

![map_shadow](./screenshots/map_shadow_test_2.jpg)

