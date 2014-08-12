#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

typedef unsigned char MAP_TYPE;
typedef MAP_TYPE *MAP;

static MAP create_map(int map_size) {
    int size = map_size * map_size * sizeof(MAP_TYPE);
    MAP map = malloc(size);
    memset(map, 0, size);
    return map;
}

static void map_set(MAP map, int str_len, int x, int y, int val) {
    int pos = y*str_len + x;
    map[pos] = val;
}

static int map_get(MAP map, int str_len, int x, int y) {
    int pos = y*str_len + x;
    return map[pos];
}

static void print_string(int str_len, unsigned int str) {
    int i;
    for (i = 0; i < str_len; i++) {
        if ((str >> i) & 1)
            printf("%c", 'H');
        else
            printf("%c", 'P');
    }
}

static void print_map(MAP map, int map_size) {\
    int y;
    for (y = 0; y < map_size; y++) {
        int x;
        for (x = 0; x < map_size; x++) {
            switch (map_get(map, map_size, x, y)) {
                case 1:
                    printf("P");
                    break;
                case 2:
                    printf("H");
                    break;
                default:
                    printf(".");
            }
        }
        printf("\n");
    }
}

static int dx[] = { -1, 0, 1, 0 };
static int dy[] = { 0, -1, 0, 1 };

static int count_bonds(MAP map, int map_size, int last_x, int last_y) {
    int i;
    
    int score = 0;
    for (i = 0; i < 4; i++) {
        if (map_get(map, map_size, last_x + dx[i], last_y + dy[i]) == 2)
            score++;
    }
    return score;
}

static int search(MAP map, int map_size, int str_len, unsigned int str, int last_x, int last_y) {
    int i;
    int best_score = -1;
    
    if (str_len == 0) {
        //print_map(map, map_size);
        return 0;
    }
    
    for (i = 0; i < 4; i++) {
        int x = last_x + dx[i];
        int y = last_y + dy[i];
        
        if (map_get(map, map_size, x, y) != 0)
            continue;
        map_set(map, map_size, x, y, (str & 1) + 1);
        int score = (str & 1) ? count_bonds(map, map_size, x, y) : 0;
        int sub_score = search(map, map_size, str_len - 1, str >> 1, x, y);
        if (sub_score != -1) {
            score += sub_score;
            if (score > best_score)
                best_score = score;
        }
        map_set(map, map_size, x, y, 0);
    }
    return best_score;
}

static int optimise(MAP map, int map_size, int str_len, unsigned int str) {
    map_set(map, map_size, str_len-1, str_len-1, (str & 1) + 1);
    str >>= 1;
    map_set(map, map_size, str_len-1, str_len, (str & 1) + 1);
    int score = (str & 1) ? count_bonds(map, map_size, str_len-1, str_len) : 0;
    
    score += search(map, map_size, str_len-2, str >> 1, str_len-1, str_len);
    
    map_set(map, map_size, str_len-1, str_len, 0);
    map_set(map, map_size, str_len-1, str_len-1, 0);
    
    return score;
}

static int get_weight(int str_len, int str) {
    int i;
    for (i = 0; i < str_len; i++) {
        int b1 = (str >> i) & 1;
        int b2 = (str >> (str_len-i-1)) & 1;
        if (b1 && !b2) return 2;
        if (b2 && !b1) return 0;
    }
    return 1;
}

int main(int argc, char *argv[])
{
    int str_len;

    if (argc < 2) {
        exit(1);
    }
    str_len = atoi(argv[1]);
    
    long int start_time = clock();
    
    int map_size = str_len*2-1;
    MAP map = create_map(map_size);
    
    int total_score = 0;
    int num_scores = 0;
    
    unsigned int str = 0;
    while (!((str >> str_len) & 1)) {
        int weight = get_weight(str_len, str);
        
        if (weight != 0) {
            int score = optimise(map, map_size, str_len, str);
            if (!((str+1) & 0xFF)) {
                //print_string(str_len, str);
                //printf("    %d   (weight %d)\n", score, weight);
                print_string(str_len - 8, str >> 8);
                printf("\n");
            }
            total_score += score * weight;
        }
        num_scores += weight;
        
        str++;
    }
    
    printf("Total score = %d\n", total_score);
    printf("Num scores = %d\n", num_scores);
    
    free(map);
    
    long int stop_time = clock();
    printf("Time = %f\n", (stop_time - start_time) / (float) CLOCKS_PER_SEC);
    
    return 0;
}
