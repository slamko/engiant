#define GL_GLEXT_PROTOTYPES 1
#define GL3_PROTOTYPES 1

#include <GL/gl.h>
#include <math.h>
#include "engine.h"

#include <GL/glu.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <GL/GLFW/glfw3.h>

#define PI 3.1415926535897

GLFWwindow *window;
GLuint vertex_shader = 0, fragment_shader = 0;
GLuint shader_program = 0;

void glfw_error_handle(int err, const char *msg) {
    fputs(msg, stderr);
}

volatile bool left_mouse_pressed;

void mouse_click_callback(GLFWwindow * win, int button, int action, int mods) {
    if (button == GLFW_MOUSE_BUTTON_LEFT && action == GLFW_RELEASE) {
        left_mouse_pressed = 1;
    }
}

void key_callback(GLFWwindow *win, int key, int scancode, int action, int mode) {
    if (scancode == 9) {
        glfwSetWindowShouldClose(win, 1);
    }
}

void debug_msg_callback(GLenum source,
            GLenum type,
            GLuint id,
            GLenum severity,
            GLsizei length,
            const GLchar *message,
            const void *userParam) {

    switch (severity) {
        
    case GL_DEBUG_SEVERITY_HIGH:;
        printf("\033[0;33m");
        break;
    case GL_DEBUG_SEVERITY_MEDIUM:;
        printf("\033[0;31m");
        break;
    }

    printf("OGL Debug Log: %s\n", message);
    printf("\033[0m");
}

#define R(...) " " #__VA_ARGS__ " "

void gl_error_handle(GLenum err) {
    /* fprintf(stderr, "GLU error: %s\n", gluErrorString(err)); */
}

const GLchar *vertex_source = 
    "#version 330 core\n"
    R(
        layout (location = 0) in vec3 position;

        uniform ivec2 window_dim;
        uniform vec3 color;
        out vec3 our_color;
        
        void main()
        {
            float x_coord = (position.x / float(window_dim.x)) * 2.0 - 1.0;
            float y_coord = -((position.y / float(window_dim.y)) * 2.0 - 1.0);
            gl_Position = vec4(x_coord, y_coord, 0.0, 1.0);
            our_color = vec3(color.x / 255.0, color.y / 255.0, color.z / 255.0);
        }
    );

const GLchar *fragment_source = 
    "#version 330 core\n"
    R(
        out vec4 color;

        in vec3 our_color;
        
        void main()
        {
            color = vec4(our_color, 1.0f);
        }
    );

int shader_handle_error(GLuint id, GLenum info_type, void (check_func)(GLuint, GLenum, GLint *)) {
    GLint res = 0;
    char info_log[0x400] = {0};

    check_func(id, info_type, &res);

    if (!res) {
        glGetShaderInfoLog(id, sizeof (info_log), NULL, info_log);
        puts(info_log);
        return 1;
    }

    return 0;
}

typedef struct {
    GLfloat x, y, z;
} GLvec3;

GLvec3 *vertices;
GLuint *indices;

int build_circle(GLuint *ebo, GLuint *vao, GLuint *vbo, size_t sector_num, float radius, float center_x, float center_y, Color color) {
    int ret = 1;

    if (sector_num < 2) {
        return ret;
    }

    size_t vertice_num = sector_num + 1;
    size_t indice_num = 3 * sector_num;

    if (!vertices) {        
        vertices = calloc(vertice_num, sizeof *vertices);
        indices = calloc(indice_num, sizeof *indices);
    }
    
    vertices[0] = (GLvec3) {.x = center_x, .y = center_y, .z = 0.0};

    for (size_t i = 0; i < sector_num; i++) {
        vertices[i + 1] = (GLvec3) {
            .x = center_x + radius * cosf(((float)i / sector_num) * 2*PI),
            .y = center_y + radius * sinf(((float)i / sector_num) * 2*PI),
            .z = 0.0};

        indices[3*i + 0] = 0;
        indices[3*i + 1] = i + 1;
        indices[3*i + 2] = (i + 2) > sector_num ? 1 : i + 2;
    }

    glGenVertexArrays(1, vao);
    glGenBuffers(1, vbo);
    glGenBuffers(1, ebo);

    glBindVertexArray(*vao);
    glBindBuffer(GL_ARRAY_BUFFER, *vbo);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, *ebo);

    glBufferData(GL_ARRAY_BUFFER, sizeof(*vertices) * vertice_num, vertices, GL_DYNAMIC_DRAW);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, sizeof(*indices) * indice_num, indices, GL_DYNAMIC_DRAW);

    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 3 * sizeof (GLfloat), NULL);
    glEnableVertexAttribArray(0);

    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);

    return ret;
}

void render_triangle (GLuint *vao, GLuint *vbo) {
    GLfloat vertices[] = {
        -0.5f, -0.5f, 0.0f, 1.0, 0.0, 0.0,
        0.5f, -0.5f, 0.0f, 0.0, 1.0, 0.0,
        0.0f,  0.5f, 0.0f, 0.0, 0.0, 1.0,
    };
 
    glGenVertexArrays(1, vao);
    glGenBuffers(1, vbo);

    glBindVertexArray(*vao);
    glBindBuffer(GL_ARRAY_BUFFER, *vbo);

    glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_STATIC_DRAW);

    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 6 * sizeof (GLfloat), (GLvoid *)0);
    glEnableVertexAttribArray(0);

    glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 6 * sizeof (GLfloat), (GLvoid *)(3 * sizeof (GLfloat)));
    glEnableVertexAttribArray(1);

    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);

}

void build_rectangle (GLuint *ebo, GLuint *vao, GLuint *vbo, int posx, int posy, int width, int height) {
    GLvec3 vertices[4] = {0};

    vertices[0] = (GLvec3){
        .x = posx + (width / 2.0),
        .y = posy + (height / 2.0),
        .z = 0.0
    };

    vertices[1] = (GLvec3){
        .x = posx - (width / 2.0),
        .y = posy + (height / 2.0),
        .z = 0.0
    };

    vertices[2] = (GLvec3){
        .x = posx + (width / 2.0),
        .y = posy - (height / 2.0),
        .z = 0.0
    };

    vertices[3] = (GLvec3){
        .x = posx - (width / 2.0),
        .y = posy - (height / 2.0),
        .z = 0.0
    };

    GLuint indices[] = {
        0, 1, 2,
        1, 2, 3,
    };

    glGenVertexArrays(1, vao);
    glGenBuffers(1, vbo);
    glGenBuffers(1, ebo);

    glBindVertexArray(*vao);
    glBindBuffer(GL_ARRAY_BUFFER, *vbo);

    glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_DYNAMIC_DRAW);

    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, *ebo);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, sizeof indices, indices, GL_DYNAMIC_DRAW);

    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 3 * sizeof (GLfloat), (GLvoid *)0);
    glEnableVertexAttribArray(0);

    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);
}

/*
int main(void) {
    
    GLuint vertex_shader = 0, fragment_shader = 0;
    GLuint shader_program = 0;

    GLuint vbo, vao, ebo;

    int width, height;
    if(glfwInit() == GLFW_FALSE) {
        printf("Error\n");
        exit(1);
    }

    glfwSetErrorCallback(&glfw_error_handle);

    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
    
    glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);
    glfwWindowHint(GLFW_RESIZABLE, GL_FALSE);

    window = glfwCreateWindow(680, 680, "Hello", NULL, NULL);
    if (!window) {
        GLenum err = glGetError();
        printf("Window create failed, error: %u\n", err);
        gl_error_handle(err);
        goto cleanup;
    }

    glfwMakeContextCurrent(window);

    glfwSetKeyCallback(window, &key_callback);
    glfwGetFramebufferSize(window, &width, &height);

    glEnable(GL_DEBUG_OUTPUT);
    glDebugMessageCallback(&debug_msg_callback, NULL);
    
    glViewport(0, 0, width, height);

    vertex_shader = glCreateShader(GL_VERTEX_SHADER);
    glShaderSource(vertex_shader, 1, &vertex_source, NULL);
    glCompileShader(vertex_shader);

    if (shader_handle_error(vertex_shader, GL_COMPILE_STATUS, glGetShaderiv)) {
        goto cleanup;
    }

    fragment_shader = glCreateShader(GL_FRAGMENT_SHADER);
    glShaderSource(fragment_shader, 1, &fragment_source, NULL);
    glCompileShader(fragment_shader);

    if (shader_handle_error(fragment_shader, GL_COMPILE_STATUS, glGetShaderiv)) {
        goto cleanup;
    }

    shader_program = glCreateProgram();
    glAttachShader(shader_program, vertex_shader);
    glAttachShader(shader_program, fragment_shader);
    glLinkProgram(shader_program);

    if (shader_handle_error(shader_program, GL_LINK_STATUS, glGetProgramiv)) {
        goto cleanup;
    }

    glDeleteShader(vertex_shader);
    glDeleteShader(fragment_shader);

    size_t sector_num = 2048;
    build_circle(&ebo, &vao, &vbo, sector_num, 0.8, 0.0, 0.0, (Color){0});

    while (!glfwWindowShouldClose(window) ) {
        glfwPollEvents();

        glClearColor(0.2f, 0.3f, 0.3f, 1.0f);
        glClear(GL_COLOR_BUFFER_BIT);

        glUseProgram(shader_program);
        glBindVertexArray(vao);

        glDrawElements(GL_TRIANGLES, 3 * sector_num, GL_UNSIGNED_INT, NULL);

        glBindVertexArray(0);
        
        glfwSwapBuffers(window);
    }

  cleanup:
    glDeleteVertexArrays(1, &vao);
    glDeleteBuffers(1, &vbo);
    glfwTerminate();

    return 0;
}
*/

bool WindowShouldClose(void) {
    return glfwWindowShouldClose(window);
}

void InitWindow(int width, int height, const char *title) {
    if(glfwInit() == GLFW_FALSE) {
        printf("Error\n");
        exit(1);
    }

    glfwSetErrorCallback(&glfw_error_handle);

    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
    
    glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);
    glfwWindowHint(GLFW_RESIZABLE, GL_FALSE);

    window = glfwCreateWindow(width, height, "Hello", NULL, NULL);
    if (!window) {
        GLenum err = glGetError();
        printf("Window create failed, error: %u\n", err);
        gl_error_handle(err);
        exit(1);
    }

    glfwMakeContextCurrent(window);

    glfwSetKeyCallback(window, &key_callback);
    glfwSetMouseButtonCallback(window, mouse_click_callback);

    glEnable(GL_DEBUG_OUTPUT);
    glDebugMessageCallback(&debug_msg_callback, NULL);
    
    glViewport(0, 0, width, height);

    vertex_shader = glCreateShader(GL_VERTEX_SHADER);
    glShaderSource(vertex_shader, 1, &vertex_source, NULL);
    glCompileShader(vertex_shader);

    if (shader_handle_error(vertex_shader, GL_COMPILE_STATUS, glGetShaderiv)) {
        exit(1);
    }

    fragment_shader = glCreateShader(GL_FRAGMENT_SHADER);
    glShaderSource(fragment_shader, 1, &fragment_source, NULL);
    glCompileShader(fragment_shader);

    if (shader_handle_error(fragment_shader, GL_COMPILE_STATUS, glGetShaderiv)) {
        exit(1);
    }

    shader_program = glCreateProgram();
    glAttachShader(shader_program, vertex_shader);
    glAttachShader(shader_program, fragment_shader);
    glLinkProgram(shader_program);

    if (shader_handle_error(shader_program, GL_LINK_STATUS, glGetProgramiv)) {
        exit(1);
    }

    glDeleteShader(vertex_shader);
    glDeleteShader(fragment_shader);
}

void CloseWindow(void) {
    glfwTerminate();
}

void BeginDrawing(void) {
    glfwPollEvents();
}

void EndDrawing(void) {
    glUseProgram(0);
    glfwSwapBuffers(window);
}

void setup_shaders(Color color) {
    int buf_width, buf_height;
    glfwGetFramebufferSize(window, &buf_width, &buf_height);

    int uniform_window_dim = glGetUniformLocation(shader_program, "window_dim");
    int uniform_color = glGetUniformLocation(shader_program, "color");

    glUseProgram(shader_program);
    glUniform2i(uniform_window_dim, buf_width, buf_height);
    glUniform3f(uniform_color, color.r, color.g, color.b);
}

void draw_primitive(size_t elements_num, GLuint ebo, GLuint vbo, GLuint vao, Color color) {
    setup_shaders(color);
    glBindVertexArray(vao);
    glDrawElements(GL_TRIANGLES, elements_num, GL_UNSIGNED_INT, NULL);
    
    glBindVertexArray(0);
    glUseProgram(0);

    glDeleteBuffers(1, &ebo);
    glDeleteBuffers(1, &vbo);
    glDeleteVertexArrays(1, &vao);
}

void DrawCircle(int centerX, int centerY, float radius, Color color) {
    GLuint ebo, vbo, vao;
    int sector_num = 32;

    build_circle(&ebo, &vao, &vbo, sector_num, radius, (float)centerX, (float)centerY, color);
    draw_primitive(sector_num * 3, ebo, vbo, vao, color);
}

void DrawPixel(int posX, int posY, Color color) {
    GLuint vao, vbo;
    GLvec3 vertices [1] = {0};

    vertices[0] = (GLvec3) {
        .x = posX,
        .y = posY,
        .z = 0.0,
    };

    setup_shaders(color);
    glGenVertexArrays(1, &vao);
    glGenBuffers(1, &vbo);

    glBindVertexArray(vao);
    glBindBuffer(GL_ARRAY_BUFFER, vbo);

    glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_DYNAMIC_DRAW);

    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 3 * sizeof (GLfloat), NULL);
    glEnableVertexAttribArray(0);

    glBindBuffer(GL_ARRAY_BUFFER, 0);

    glDrawBuffer(GL_FRONT);

    glBindVertexArray(0);
    glUseProgram(0);

    glDeleteBuffers(1, &vbo);
    glDeleteVertexArrays(1, &vao);
}

void DrawRectangle(int posX, int posY, int width, int height, Color color) {
    GLuint ebo, vbo, vao;

    build_rectangle(&ebo, &vao, &vbo, posX, posY, width, height);
    draw_primitive(6, ebo, vbo, vao, color);
}

void DrawLine(int startPosX, int startPosY, int endPosX, int endPosY, Color color) {
    GLuint vao, vbo;
    GLvec3 vertices [2] = {0};

    vertices[0] = (GLvec3) {
        .x = startPosX,
        .y = startPosY,
        .z = 0.0,
    };

    vertices[1] = (GLvec3) {
        .x = endPosX,
        .y = endPosY,
        .z = 0.0,
    };

    setup_shaders(color);
    glGenVertexArrays(1, &vao);
    glGenBuffers(1, &vbo);

    glBindVertexArray(vao);
    glBindBuffer(GL_ARRAY_BUFFER, vbo);

    glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_DYNAMIC_DRAW);

    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 3 * sizeof (GLfloat), NULL);
    glEnableVertexAttribArray(0);

    glBindBuffer(GL_ARRAY_BUFFER, 0);

    glDrawArrays(GL_LINES, 0, 2);

    glBindVertexArray(0);
    glUseProgram(0);

    glDeleteBuffers(1, &vbo);
    glDeleteVertexArrays(1, &vao);

/*
    glBegin(GL_LINES);
    glColor4ub(color.r, color.g, color.b, color.a);
    glVertex2f((float)startPosX, (float)startPosY);
    glVertex2f((float)endPosX, (float)endPosY);
        glEnd();
*/

}


void DrawLine3D(Vector3 startPos, Vector3 endPos, Color color) {
    GLuint vao, vbo;
    Vector3 vertices [2] = {0};

    vertices[0] = startPos;

    vertices[1] = endPos;

    setup_shaders(color);
    glGenVertexArrays(1, &vao);
    glGenBuffers(1, &vbo);

    glBindVertexArray(vao);
    glBindBuffer(GL_ARRAY_BUFFER, vbo);

    glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_DYNAMIC_DRAW);

    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 3 * sizeof (GLfloat), NULL);
    glEnableVertexAttribArray(0);

    glBindBuffer(GL_ARRAY_BUFFER, 0);

    glDrawArrays(GL_LINES, 0, 2);

    glBindVertexArray(0);
    glUseProgram(0);

    glDeleteBuffers(1, &vbo);
    glDeleteVertexArrays(1, &vao);

/*
    glBegin(GL_LINES);
    glColor4ub(color.r, color.g, color.b, color.a);
    glVertex2f((float)startPosX, (float)startPosY);
    glVertex2f((float)endPosX, (float)endPosY);
        glEnd();
*/
}

GLvec3 normalize_color(Color color) {

    return (GLvec3){.x = color.r / 255.0, .y = color.g / 255.0, .z = color.b / 255.0};
}

void ClearBackground(Color color) {
    GLvec3 norm = normalize_color(color);
    glClearColor(norm.x, norm.y, norm.z, 1.0);
    glClear(GL_COLOR_BUFFER_BIT);

}

unsigned int FPS;

void SetTargetFPS(int fps) {
    FPS = fps;
}

int GetFPS(void) {
    return FPS;
}

float GetFrameTime(void) {
    return 1.0 / (float)FPS;
}

void DrawCircleSector(Vector2 center, float radius, float startAngle, float endAngle, int segments, Color color) {
    DrawCircle(center.x, center.y, radius, color);
}

bool IsMouseButtonReleased(int button) {
    bool val = left_mouse_pressed;
    left_mouse_pressed = false;
    return val;
}

Vector2 GetMousePosition(void) { 
    double xpos, ypos;
    glfwGetCursorPos(window, &xpos, &ypos);

    printf("%f %f\n", xpos, ypos);
    return (Vector2) {xpos, ypos };
}
