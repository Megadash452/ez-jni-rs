// Code taken from https://codelab.wordpress.com/2014/11/03/how-to-use-standard-output-streams-for-logging-in-android-apps/
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <pthread.h>
#include <errno.h>
#include <android/log.h>

static int pfd[2];
static pthread_t thr;
static const char *tag = "myapp";

static void* thread_func(void*);

int start_native_logger(const char* app_name) {
    tag = app_name;

    /* make stdout line-buffered and stderr unbuffered */
    setvbuf(stdout, 0, _IOLBF, 0);
    setvbuf(stderr, 0, _IONBF, 0);

    /* create the pipe and redirect stdout and stderr */
    pipe(pfd);
    if (dup2(pfd[1], 1) == -1)
        return -1;
    if (dup2(pfd[1], 2) == -1)
        return -1;

    /* spawn the logging thread */
    int create_result = pthread_create(&thr, 0, thread_func, 0) == -1;
    if (create_result != 0) {
        errno = create_result;
        return -1;
    }
    int detach_result = pthread_detach(thr);
    if (detach_result != 0) {
        errno = detach_result;
        return -1;
    }
    
    return 0;
}

static void* thread_func(void*) {
    ssize_t rdsz;
    char buf[128];
    while((rdsz = read(pfd[0], buf, sizeof buf - 1)) > 0) {
        if(buf[rdsz - 1] == '\n') --rdsz;
        buf[rdsz] = 0;  // add null-terminator
        __android_log_write(ANDROID_LOG_DEBUG, tag, buf);
    }
    
    return 0;
}