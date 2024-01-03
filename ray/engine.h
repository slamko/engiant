#ifndef ENGINE_H
#define ENGINE_H

typedef struct {
  float x, y, z;
} Vector3;

typedef struct Vector2 {
  float x, y;
} Vector2;

typedef struct Color {
  unsigned char r, g, b, a;
} Color;

typedef struct Rectangle {
  float x, y;
  float width, height;
} Rectangle;

#include <stdbool.h>
void SetTargetFPS(int fps); //设置目标FPS(最大值)
int GetFPS(void);           //获取当前FPS
float GetFrameTime(void); //获取绘制的最后一帧的时间(以秒为单位)(增量时间)
double GetTime(void); //获取自InitWindow()以来的运行时间(秒)

void InitWindow(int width, int height, const char *title);
bool WindowShouldClose(void);
void CloseWindow(void);
// Drawing-related functions
void ClearBackground(
    Color color);        // Set background color (framebuffer clear color)
void BeginDrawing(void); // Setup canvas (framebuffer) to start drawing
void EndDrawing(void);
void DrawPixel(int posX, int posY, Color color); // Draw a pixel
void DrawPixelV(Vector2 position, Color color); // Draw a pixel (Vector version)
void DrawLine(int startPosX, int startPosY, int endPosX, int endPosY,
              Color color); // Draw a line
void DrawLineV(Vector2 startPos, Vector2 endPos,
               Color color); // Draw a line (using gl lines)
void DrawLineEx(Vector2 startPos, Vector2 endPos, float thick,
                Color color); // Draw a line (using triangles/quads)
void DrawLineStrip(Vector2 *points, int pointCount,
                   Color color); // Draw lines sequence (using gl lines)
void DrawLineBezier(
    Vector2 startPos, Vector2 endPos, float thick,
    Color color); // Draw line segment cubic-bezier in-out interpolation
void DrawCircle(int centerX, int centerY, float radius,
                Color color); // Draw a color-filled circle
void DrawCircleSector(Vector2 center, float radius, float startAngle,
                      float endAngle, int segments,
                      Color color); // Draw a piece of a circle
void DrawCircleSectorLines(Vector2 center, float radius, float startAngle,
                           float endAngle, int segments,
                           Color color); // Draw circle sector outline
void DrawCircleGradient(int centerX, int centerY, float radius, Color color1,
                        Color color2); // Draw a gradient-filled circle
void DrawCircleV(Vector2 center, float radius,
                 Color color); // Draw a color-filled circle (Vector version)
void DrawCircleLines(int centerX, int centerY, float radius,
                     Color color); // Draw circle outline
void DrawCircleLinesV(Vector2 center, float radius,
                      Color color); // Draw circle outline (Vector version)
void DrawEllipse(int centerX, int centerY, float radiusH, float radiusV,
                 Color color); // Draw ellipse
void DrawEllipseLines(int centerX, int centerY, float radiusH, float radiusV,
                      Color color); // Draw ellipse outline
void DrawRing(Vector2 center, float innerRadius, float outerRadius,
              float startAngle, float endAngle, int segments,
              Color color); // Draw ring
void DrawRingLines(Vector2 center, float innerRadius, float outerRadius,
                   float startAngle, float endAngle, int segments,
                   Color color); // Draw ring outline
void DrawRectangle(int posX, int posY, int width, int height,
                   Color color); // Draw a color-filled rectangle
void DrawRectangleV(
    Vector2 position, Vector2 size,
    Color color); // Draw a color-filled rectangle (Vector version)
void DrawRectangleRec(Rectangle rec,
                      Color color); // Draw a color-filled rectangle
void DrawRectanglePro(
    Rectangle rec, Vector2 origin, float rotation,
    Color color); // Draw a color-filled rectangle with pro parameters
void DrawRectangleGradientV(
    int posX, int posY, int width, int height, Color color1,
    Color color2); // Draw a vertical-gradient-filled rectangle
void DrawRectangleGradientH(
    int posX, int posY, int width, int height, Color color1,
    Color color2); // Draw a horizontal-gradient-filled rectangle
void DrawRectangleGradientEx(
    Rectangle rec, Color col1, Color col2, Color col3,
    Color col4); // Draw a gradient-filled rectangle with custom vertex colors
void DrawRectangleLines(int posX, int posY, int width, int height,
                        Color color); // Draw rectangle outline
void DrawRectangleLinesEx(
    Rectangle rec, float lineThick,
    Color color); // Draw rectangle outline with extended parameters
void DrawRectangleRounded(Rectangle rec, float roundness, int segments,
                          Color color); // Draw rectangle with rounded edges
void DrawRectangleRoundedLines(
    Rectangle rec, float roundness, int segments, float lineThick,
    Color color); // Draw rectangle with rounded edges outline
void DrawTriangle(Vector2 v1, Vector2 v2, Vector2 v3,
                  Color color); // Draw a color-filled triangle (vertex in
                                // counter-clockwise order!)
void DrawTriangleLines(
    Vector2 v1, Vector2 v2, Vector2 v3,
    Color color); // Draw triangle outline (vertex in counter-clockwise order!)
void DrawTriangleFan(Vector2 *points, int pointCount,
                     Color color); // Draw a triangle fan defined by points
                                   // (first vertex is the center)
void DrawTriangleStrip(Vector2 *points, int pointCount,
                       Color color); // Draw a triangle strip defined by points
void DrawPoly(Vector2 center, int sides, float radius, float rotation,
              Color color); // Draw a regular polygon (Vector version)
void DrawPolyLines(Vector2 center, int sides, float radius, float rotation,
                   Color color); // Draw a polygon outline of n sides
void DrawPolyLinesEx(
    Vector2 center, int sides, float radius, float rotation, float lineThick,
    Color color); // Draw a polygon outline of n sides with extended parameters

// Basic shapes collision detection functions
bool CheckCollisionRecs(
    Rectangle rec1, Rectangle rec2); // Check collision between two rectangles
bool CheckCollisionCircles(
    Vector2 center1, float radius1, Vector2 center2,
    float radius2); // Check collision between two circles
bool CheckCollisionCircleRec(
    Vector2 center, float radius,
    Rectangle rec); // Check collision between circle and rectangle
bool CheckCollisionPointRec(
    Vector2 point, Rectangle rec); // Check if point is inside rectangle
bool CheckCollisionPointCircle(Vector2 point, Vector2 center,
                               float radius); // Check if point is inside circle
bool CheckCollisionPointTriangle(
    Vector2 point, Vector2 p1, Vector2 p2,
    Vector2 p3); // Check if point is inside a triangle
bool CheckCollisionPointPoly(
    Vector2 point, Vector2 *points,
    int pointCount); // Check if point is within a polygon described by array of
                     // vertices
bool CheckCollisionLines(
    Vector2 startPos1, Vector2 endPos1, Vector2 startPos2, Vector2 endPos2,
    Vector2 *
        collisionPoint); // Check the collision between two lines defined by two
                         // points each, returns collision point by reference
bool CheckCollisionPointLine(
    Vector2 point, Vector2 p1, Vector2 p2,
    int threshold); // Check if point belongs to line created between two points
                    // [p1] and [p2] with defined margin in pixels [threshold]
Rectangle GetCollisionRec(
    Rectangle rec1,
    Rectangle rec2); // Get collision rectangle for two rectangles collision

bool IsKeyPressed(int key); // Check if a key has been pressed once
bool IsKeyPressedRepeat(
    int key); // Check if a key has been pressed again (Only PLATFORM_DESKTOP)
bool IsKeyDown(int key);     // Check if a key is being pressed
bool IsKeyReleased(int key); // Check if a key has been released once
bool IsKeyUp(int key);       // Check if a key is NOT being pressed
int GetKeyPressed(void);  // Get key pressed (keycode), call it multiple times
                          // for keys queued, returns 0 when the queue is empty
int GetCharPressed(void); // Get char pressed (unicode), call it multiple times
                          // for chars queued, returns 0 when the queue is empty
void SetExitKey(int key); // Set a custom key to exit program (default is ESC)

// Input-related functions: mouse
bool IsMouseButtonPressed(
    int button); // Check if a mouse button has been pressed once
bool IsMouseButtonDown(int button); // Check if a mouse button is being pressed
bool IsMouseButtonReleased(
    int button); // Check if a mouse button has been released once
bool IsMouseButtonUp(
    int button);                // Check if a mouse button is NOT being pressed
int GetMouseX(void);            // Get mouse position X
int GetMouseY(void);            // Get mouse position Y
Vector2 GetMousePosition(void); // Get mouse position XY
Vector2 GetMouseDelta(void);    // Get mouse delta between frames
void SetMousePosition(int x, int y);            // Set mouse position XY
void SetMouseOffset(int offsetX, int offsetY);  // Set mouse offset
void SetMouseScale(float scaleX, float scaleY); // Set mouse scaling
float GetMouseWheelMove(
    void); // Get mouse wheel movement for X or Y, whichever is larger
Vector2 GetMouseWheelMoveV(void); // Get mouse wheel movement for both X and Y
void SetMouseCursor(int cursor);
void DrawLine3D(Vector3 startPos, Vector3 endPos,
                Color color); // Draw a line in 3D world space
void DrawPoint3D(
    Vector3 position,
    Color color); // Draw a point in 3D space, actually a small line
void DrawCircle3D(Vector3 center, float radius, Vector3 rotationAxis,
                  float rotationAngle,
                  Color color); // Draw a circle in 3D world space
void DrawTriangle3D(Vector3 v1, Vector3 v2, Vector3 v3,
                    Color color); // Draw a color-filled triangle (vertex in
                                  // counter-clockwise order!)
void DrawTriangleStrip3D(
    Vector3 *points, int pointCount,
    Color color); // Draw a triangle strip defined by points
void DrawCube(Vector3 position, float width, float height, float length,
              Color color); // Draw cube
void DrawCubeV(Vector3 position, Vector3 size,
               Color color); // Draw cube (Vector version)
void DrawCubeWires(Vector3 position, float width, float height, float length,
                   Color color); // Draw cube wires
void DrawCubeWiresV(Vector3 position, Vector3 size,
                    Color color); // Draw cube wires (Vector version)
void DrawSphere(Vector3 centerPos, float radius, Color color); // Draw sphere
void DrawSphereEx(Vector3 centerPos, float radius, int rings, int slices,
                  Color color); // Draw sphere with extended parameters
void DrawSphereWires(Vector3 centerPos, float radius, int rings, int slices,
                     Color color); // Draw sphere wires
void DrawCylinder(Vector3 position, float radiusTop, float radiusBottom,
                  float height, int slices,
                  Color color); // Draw a cylinder/cone
void DrawCylinderEx(
    Vector3 startPos, Vector3 endPos, float startRadius, float endRadius,
    int sides,
    Color color); // Draw a cylinder with base at startPos and top at endPos
void DrawCylinderWires(Vector3 position, float radiusTop, float radiusBottom,
                       float height, int slices,
                       Color color); // Draw a cylinder/cone wires
void DrawCylinderWiresEx(Vector3 startPos, Vector3 endPos, float startRadius,
                         float endRadius, int sides,
                         Color color); // Draw a cylinder wires with base at
                                       // startPos and top at endPos
void DrawCapsule(Vector3 startPos, Vector3 endPos, float radius, int slices,
                 int rings,
                 Color color); // Draw a capsule with the center of its sphere
                               // caps at startPos and endPos
void DrawCapsuleWires(Vector3 startPos, Vector3 endPos, float radius,
                      int slices, int rings,
                      Color color); // Draw capsule wireframe with the center of
                                    // its sphere caps at startPos and endPos
void DrawPlane(Vector3 centerPos, Vector2 size, Color color); // Draw a plane XZ
void DrawGrid(int slices, float spacing);

#endif
