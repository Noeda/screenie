#include <sys/ioctl.h>
#include <termios.h>
#include <unistd.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

int get_window_size(int32_t* width, int32_t* height)
{
    struct winsize ws;
    if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws)) {
        return errno;
    }

    (*width)  = ws.ws_col;
    (*height) = ws.ws_row;

    return 0;
}

struct termios* set_raw_terminal_mode(void)
{
    struct termios* to = malloc(sizeof(struct termios));
    if ( !to ) {
        return NULL;
    }
    if (tcgetattr(STDOUT_FILENO, to)) {
        free(to);
        return NULL;
    }

    struct termios new_to;
    memcpy(&new_to, to, sizeof(struct termios));

    new_to.c_lflag &= ~(ECHO|ICANON);
    if (tcsetattr(STDOUT_FILENO, TCSADRAIN, &new_to)) {
        free(to);
        return NULL;
    }

    return to;
}

void restore_terminal_mode(struct termios* to)
{
    // Ignore errors; what could we do anyway?
    tcsetattr(STDOUT_FILENO, TCSADRAIN, to);
    free(to);
}

