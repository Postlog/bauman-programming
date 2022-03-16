import glfw
from OpenGL.GL import *
from math import *
from random import uniform

def random_color():
    return [
        uniform(0, 1),
        uniform(0, 1),
        uniform(0, 1), 
        1
    ]

def random_color_set(l):
    return [random_color() for _ in range(l)]

verticies = [[0 for i in range(3)] for j in range(12)]
indicies = [[0 for i in range(3)] for j in range(20)]
colors = random_color_set(12)

k = pi/180

x_angle = y_angle = 0
scale = 1

def init_indicies(indicies):
    indicies[0][0] = 0 
    indicies[0][1] = 2 
    indicies[0][2] = 1
 
    indicies[1][0] = 0
    indicies[1][1] = 3
    indicies[1][2] = 2
 
    indicies[2][0] = 0
    indicies[2][1] = 4
    indicies[2][2] = 3
 
    indicies[3][0] = 0
    indicies[3][1] = 5
    indicies[3][2] = 4
 
    indicies[4][0] = 0
    indicies[4][1] = 1
    indicies[4][2] = 5
 
    indicies[5][0] = 6
    indicies[5][1] = 1
    indicies[5][2] = 7
 
    indicies[6][0] = 7
    indicies[6][1] = 1
    indicies[6][2] = 2
 
    indicies[7][0] = 7
    indicies[7][1] = 2
    indicies[7][2] = 8
 
    indicies[8][0] = 8
    indicies[8][1] = 2
    indicies[8][2] = 3
 
    indicies[9][0] = 8
    indicies[9][1] = 3
    indicies[9][2] = 9
 
    indicies[10][0] = 9
    indicies[10][1] = 3
    indicies[10][2] = 4
 
    indicies[11][0] = 9
    indicies[11][1] = 4
    indicies[11][2] = 10
 
    indicies[12][0] = 10
    indicies[12][1] = 4
    indicies[12][2] = 5
 
    indicies[13][0] = 10
    indicies[13][1] = 5
    indicies[13][2] = 6
 
    indicies[14][0] = 6
    indicies[14][1] = 5
    indicies[14][2] = 1
 
    indicies[15][0] = 7
    indicies[15][1] = 11
    indicies[15][2] = 6
 
    indicies[16][0] = 8
    indicies[16][1] = 11
    indicies[16][2] = 7
 
    indicies[17][0] = 9
    indicies[17][1] = 11
    indicies[17][2] = 8
 
    indicies[18][0] = 10
    indicies[18][1] = 11
    indicies[18][2] = 9
 
    indicies[19][0] = 6
    indicies[19][1] = 11
    indicies[19][2] = 10

def init_verticies(R, verticies):
    a = 4 * R / sqrt(10 + 2 * sqrt(5))
    alpha = acos((1 - a * a / 2 / R / R))

    verticies[0][0] = 0
    verticies[0][1] = 0
    verticies[0][2] = R
 
    verticies[1][0] = R*sin(alpha)*sin(0)
    verticies[1][1] = R*sin(alpha)*cos(0)
    verticies[1][2] = R*cos(alpha)
 
    verticies[2][0] = R*sin(alpha)*sin(72*k)
    verticies[2][1] = R*sin(alpha)*cos(72*k)
    verticies[2][2] = R*cos(alpha)
 
    verticies[3][0] = R*sin(alpha)*sin(2*72*k)
    verticies[3][1] = R*sin(alpha)*cos(2*72*k)
    verticies[3][2] = R*cos(alpha)
 
    verticies[4][0] = R*sin(alpha)*sin(3*72*k)
    verticies[4][1] = R*sin(alpha)*cos(3*72*k)
    verticies[4][2] = R*cos(alpha)
 
    verticies[5][0] = R*sin(alpha)*sin(4*72*k)
    verticies[5][1] = R*sin(alpha)*cos(4*72*k)
    verticies[5][2] = R*cos(alpha)
 
    verticies[6][0] = R*sin(pi-alpha)*sin(-36*k)
    verticies[6][1] = R*sin(pi-alpha)*cos(-36*k)
    verticies[6][2] = R*cos(pi-alpha)
 
    verticies[7][0] = R*sin(pi-alpha)*sin(36*k)
    verticies[7][1] = R*sin(pi-alpha)*cos(36*k)
    verticies[7][2] = R*cos(pi-alpha)
 
    verticies[8][0] = R*sin(pi-alpha)*sin((36+72)*k)
    verticies[8][1] = R*sin(pi-alpha)*cos((36+72)*k)
    verticies[8][2] = R*cos(pi-alpha)
 
    verticies[9][0] = R*sin(pi-alpha)*sin((36+2*72)*k)
    verticies[9][1] = R*sin(pi-alpha)*cos((36+2*72)*k)
    verticies[9][2] = R*cos(pi-alpha)
 
    verticies[10][0] = R*sin(pi-alpha)*sin((36+3*72)*k)
    verticies[10][1] = R*sin(pi-alpha)*cos((36+3*72)*k)
    verticies[10][2] = R*cos(pi-alpha)
 
    verticies[11][0] = 0
    verticies[11][1] = 0
    verticies[11][2] = -R

def main():
    if not glfw.init():
        return

    window = glfw.create_window(640, 640, "Lab3", None, None)
    if not window:
        glfw.terminate()
        return

    glfw.make_context_current(window)
    init_indicies(indicies)
    init_verticies(0.5, verticies)
    glfw.set_key_callback(window, callback)
    while not glfw.window_should_close(window):
        display(window)

    glfw.destroy_window(window)
    glfw.terminate()

def callback(window, key, scancode, action, mods):
    global x_angle, y_angle, scale, colors
    if key == glfw.KEY_A:
        y_angle -= 2
    if key == glfw.KEY_D:
        y_angle += 2

    if key == glfw.KEY_W:
        x_angle -= 2
    if key == glfw.KEY_S:
        x_angle += 2

    if key == glfw.KEY_H:
        scale += 0.05
    if key == glfw.KEY_J:
        scale -= 0.05

    if action == glfw.PRESS and key == glfw.KEY_C:
        colors = random_color_set(12)

def display(window):
    glClearColor(1.0, 1.0, 1.0, 1.0)
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
    glEnable(GL_DEPTH_TEST)
    glMatrixMode(GL_MODELVIEW)
    glLoadIdentity()

    glRotatef(x_angle, 1.0, 0, 0)
    glRotatef(y_angle, 0, 1.0, 0)
    draw_axis()

    glPushMatrix()
    glScalef(scale, scale, scale)

    glEnableClientState(GL_VERTEX_ARRAY)
    glEnableClientState(GL_COLOR_ARRAY)
    glVertexPointer(3, GL_FLOAT, 0, verticies)
    glColorPointer(4, GL_FLOAT, 0, colors)
    glDrawElements(GL_TRIANGLES, 60, GL_UNSIGNED_INT, indicies);
    glPopMatrix()

    glfw.swap_buffers(window)
    glfw.poll_events()

def draw_axis():
    glLineWidth(3.0)
    
    glBegin(GL_LINES)
    glColor4f(1.0, 0.0, 0.0, 1.0) # x - red
    glVertex3f( 1.0,  0.0,  0.0) 
    glVertex3f(-1.0,  0.0,  0.0) 

    glColor4f(0.0, 0.5, 0.0, 1.0) # y - green
    glVertex3f( 0.0,  1.0,  0.0)
    glVertex3f( 0.0, -1.0,  0.0)


    glColor4f(0.0, 0.0, 1.0, 1.0) # z - blue
    glVertex3f( 0.0,  0.0,  1.0)
    glVertex3f( 0.0, 0.0,  -1.0)
    glEnd()


if __name__ == '__main__':
    main()

