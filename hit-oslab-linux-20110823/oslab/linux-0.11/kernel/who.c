#include <asm/segment.h>
#include <errno.h>
#include <string.h>

char _myname[24];

int sys_iam(const char *name)
{
    char str[25];
    int i = 0;

    do
    {
        // get char from user input
        str[i] = get_fs_byte(name + i);
    } while (i <= 25 && str[i++] != '\0');

    if (i > 24)
    {
        errno = EINVAL;
        i = -1;
    }
    else
    {
        // copy from user mode to kernel mode
        strcpy(_myname, str);
    }

    return i;
}

int sys_whoami(char *name, unsigned int size)
{
    int length = strlen(_myname);
    printk("%s\n", _myname);

    if (size < length)
    {
        errno = EINVAL;
        length = -1;
    }
    else
    {
        int i = 0;
        for (i = 0; i < length; i++)
        {
            // copy from kernel mode to user mode
            put_fs_byte(_myname[i], name + i);
        }
    }
    return length;
}
