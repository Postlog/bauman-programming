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

def main():
    if not glfw.init():
        return

    window = glfw.create_window(640, 640, "Lab2", None, None)
    if not window:
        glfw.terminate()
        return

    glfw.make_context_current(window)
    drawer = Drawer(0.2)
    renderer = Renderer(window, drawer)
    glfw.set_key_callback(window, get_key_callback(renderer))
    while not glfw.window_should_close(window):
        renderer.default_scene()

    glfw.destroy_window(window)
    glfw.terminate()


class Renderer:
    def __init__(self, window, drawer):
        self._window = window
        self._angles = [0, 0, 0] # angle_x, angle_y, angle_z
        self._drawer = drawer
        self._scale = 1
        
        self._modes = [GL_LINE, GL_FILL]
        self._mode_pointer = 0

        self._color_set_1 = random_color_set(6)
        self._color_set_2 = random_color_set(6)

    def change_colors(self):
        self._color_set_1 = random_color_set(6)
        self._color_set_2 = random_color_set(6)        

    def change_angles(self, *, x=None, y=None, z=None):
        if x:
            self._angles[0] += x
        if y:
            self._angles[1] += y
        if z:
            self._angles[2] += z

    def set_scale(self, scale):
        self._scale = scale

    def get_scale(self):    
        return self._scale

    def switch_polygon_mode(self):
        glPolygonMode(GL_FRONT_AND_BACK, self._modes[self._mode_pointer])
        self._mode_pointer = (self._mode_pointer + 1) % len(self._modes)

    def _apply_angles(self):
        glRotatef(self._angles[0], 1.0, 0, 0)
        glRotatef(self._angles[1], 0, 1.0, 0)
        glRotatef(self._angles[2], 0, 0, 1.0) 
            

    def default_scene(self):
        glClearColor(1.0, 1.0, 1.0, 1.0)
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
        glEnable(GL_DEPTH_TEST)
        glMatrixMode(GL_MODELVIEW)
        glPushMatrix()
        glLoadIdentity()
        glTranslatef(0.5, 0, 0)
        glScale(*([self._scale] * 3))
        self._apply_angles()
        self._drawer.draw_cube(self._color_set_1)

        glPushMatrix()
        glLoadIdentity()
        glTranslatef(-0.5, 0, 0.5)
        glRotatef(45, 1, 0, 0)
        glRotatef(45, 0, 1, 0)
        
        self._drawer.draw_cube(self._color_set_2)

        glPopMatrix()
        glPopMatrix()

        glfw.swap_buffers(self._window)
        glfw.poll_events()


class Drawer:
    DEFAULT_COLOR_SET = [
        [1, 0, 0, 1],
        [0, 1, 0, 1],
        [0, 0, 1, 1],
        [0.5, 0.5, 0, 1],
        [0, 0.5, 0.5, 1],
        [0.5, 0, 0.5, 1],
    ]

    def __init__(self, size=1):
        self._size = size

    def set_size(self, size):
        self._size = size

    def draw_cube(self, colors=None):
        if not colors:
            colors = Drawer.DEFAULT_COLOR_SET
        s = self._size
        glBegin(GL_POLYGON)
        glColor3f(*colors[0])
        glVertex3f(-s, -s, -s)
        glVertex3f(-s, s, -s)
        glVertex3f(-s, s, s)
        glVertex3f(-s, -s, s)
        glEnd()

        glBegin(GL_POLYGON)
        glColor3f(*colors[1])
        glVertex3f(s, -s, -s)
        glVertex3f(s, -s, s)
        glVertex3f(s, s, s)
        glVertex3f(s, s, -s)
        glEnd()

        glBegin(GL_POLYGON)
        glColor3f(*colors[2])
        glVertex3f(-s, -s, -s)
        glVertex3f(-s, -s, s)
        glVertex3f(s, -s, s)
        glVertex3f(s, -s, -s)
        glEnd()

        glBegin(GL_POLYGON)
        glColor3f(*colors[3])
        glVertex3f(-s, s, -s)
        glVertex3f(-s, s, s)
        glVertex3f(s, s, s)
        glVertex3f(s, s, -s)
        glEnd()

        glBegin(GL_POLYGON)
        glColor3f(*colors[4])
        glVertex3f(-s, -s, -s)
        glVertex3f(s, -s, -s)
        glVertex3f(s, s, -s)
        glVertex3f(-s, s, -s)
        glEnd()

        glBegin(GL_POLYGON)
        glColor3f(*colors[5])
        glVertex3f(-s, -s, s)
        glVertex3f(s, -s, s)
        glVertex3f(s, s, s)
        glVertex3f(-s, s, s)
        glEnd()
    

def get_key_callback(renderer):
    def callback(window, key, scancode, action, mods):
        if key == glfw.KEY_H:
            renderer.set_scale(renderer.get_scale() * 1.05)
        if key == glfw.KEY_J:
            renderer.set_scale(renderer.get_scale() * 0.95)
        if key == glfw.KEY_RIGHT:
            renderer.change_angles(z=-2)
        if key == glfw.KEY_LEFT:
            renderer.change_angles(z=2)
        if key == glfw.KEY_A:
            renderer.change_angles(y=2)
        if key == glfw.KEY_D:
            renderer.change_angles(y=-2)
        if key == glfw.KEY_W:
            renderer.change_angles(x=2)
        if key == glfw.KEY_S:
            renderer.change_angles(x=-2)
        if action == glfw.PRESS and key == glfw.KEY_M:
            renderer.switch_polygon_mode()
        if action == glfw.PRESS and key == glfw.KEY_C:
            renderer.change_colors()

    return callback


main()



