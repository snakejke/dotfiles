#include <X11/Xlib.h>
#include <X11/extensions/shape.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#define BORDER_WIDTH 2           // Толщина рамки в пикселях
#define BORDER_COLOR 0xFF0000   // Красный цвет (0xRRGGBB)

int main(int argc, char *argv[]) {
    if (argc != 5) {
        fprintf(stderr, "Usage: %s x y width height\n", argv[0]);
        return 1;
    }

    int x = atoi(argv[1]);
    int y = atoi(argv[2]);
    int width = atoi(argv[3]);
    int height = atoi(argv[4]);

    Display *dpy = XOpenDisplay(NULL);
    if (!dpy) return 1;

    int screen = DefaultScreen(dpy);
    Window root = RootWindow(dpy, screen);

    // Создаём окно чуть больше видео, чтобы рамка была снаружи
    int border = BORDER_WIDTH;
    XSetWindowAttributes attrs;
    attrs.override_redirect = True;
    attrs.background_pixel = BORDER_COLOR;

    Window win = XCreateWindow(dpy, root,
                               x - border, y - border,
                               width + 2*border, height + 2*border,
                               0, CopyFromParent, CopyFromParent,
                               CopyFromParent, CWOverrideRedirect | CWBackPixel, &attrs);

    // Создаём область для XShape
    Region whole_region = XCreateRegion();
    Region hole_region = XCreateRegion();

    // Внешний прямоугольник (всё окно)
    XRectangle outer_rect = {0, 0, width + 2*border, height + 2*border};
    XUnionRectWithRegion(&outer_rect, whole_region, whole_region);

    // Внутренний прямоугольник (дырка) — совпадает с зоной видео
    XRectangle inner_rect = {border, border, width, height};
    XUnionRectWithRegion(&inner_rect, hole_region, hole_region);

    // Вычитаем внутренний прямоугольник из внешнего
    XSubtractRegion(whole_region, hole_region, whole_region);

    // Применяем форму окна
    XShapeCombineRegion(dpy, win, ShapeBounding, 0, 0, whole_region, ShapeSet);

    // Показываем окно
    XMapWindow(dpy, win);
    XFlush(dpy);

    // Бесконечный цикл, чтобы окно не закрывалось
    while (1) {
        XEvent e;
        XNextEvent(dpy, &e);
        // Можно добавить обработку закрытия через событие
    }

    XDestroyWindow(dpy, win);
    XCloseDisplay(dpy);
    return 0;
}
