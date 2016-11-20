#ifndef W_OPTIONS_H
#define W_OPTIONS_H


/* MUST BE DEFINED: */
/* #define OPTION_LOG_LEVEL_INITIALIZATION LOG_LEVEL_MAX */
#define OPTION_LOG_LEVEL_INITIALIZATION 2
#define OPTION_LOG_LEVEL_EXECUTION 6

#define OPTION_USE_READLINE 1

#define OPTION_DEBUG 1
#define OPTION_DEBUG_MEMORY 1

#define OPTION_PROFILE 0

#define OPTION_PROGRAM_NAME "worker"
#define OPTION_DEFAULT_PORT 9999

#define OPTION_HEADER_SIZE 512
#define OPTION_HEADER_TIMEOUT 100
#define OPTION_BODY_TIMEOUT 1000

#define OPTION_DEFAULT_FPS 30
#define OPTION_MAX_FAST_RUNS 10

#define OPTION_DEFAULT_LAYER 0
#define OPTION_RESOURCE_PERCENTAGE 80

#define OPTION_RESOURCE_MODIFICATION_BLEED 5

/* 44100 or 22050 */
#define OPTION_SOUND_FREQUENCY 22050
#define OPTION_SOUND_FORMAT MIX_DEFAULT_FORMAT
/* Sound output channels, not mixing channels! */
/* 2 for stereo, 1 for mono */
#define OPTION_SOUND_OUTPUT_CHANNELS 2
#define OPTION_SOUND_CHUNKSIZE 2048
#define OPTION_SOUND_MIXING_CHANNELS 32

#endif
