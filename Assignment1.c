#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <time.h>
#include <sys/time.h>
#include <math.h>
#include <unistd.h>

struct thread_data
{
    int thread_ID;
    double **matrix; 
    int *index; 
    double *result; 
    int N;
    int M;
    int* indexOfBlock; 
    int B;
    int partition; 
    int singleT_B;
    int extraT_B;
    int singleT_E;
    int extraT_E;
};

//baseline algorithm
void *baseline(void *threading){
    int ID, N, M, singleT_B, extraT_B;
    double **matrix, *result;
    int *index;
    struct thread_data *Mydata;

    Mydata = (struct thread_data *) threading;
    ID = Mydata ->thread_ID;
    matrix = Mydata -> matrix;
    index = Mydata -> index;
    result = Mydata -> result;
    M = Mydata -> M;
    N = Mydata -> N;
    singleT_B = Mydata -> singleT_B;
    extraT_B = Mydata -> extraT_B;
    /*
    start: The location that starts in the result matrix
    mission: The number of pairs that thread should calculate.
    startI/startJ: Coordinates of the first pair
    */
    int start = 0, mission = 0, startI = 0, startJ = 0;

    // If ID smaller than the extraT, it means that this thread has more mission.
    if (ID < extraT_B)
    {
        start = ID*(singleT_B+1);
        mission = singleT_B + 1;
    } else{
        start = ID *singleT_B + extraT_B;
        mission = singleT_B;
    }
    // The situation that thread have no task.
    if (mission == 0)
    {
        return NULL;
    }

    // calculate the startI and startJ
    for (int i = 0; i < N; i++)
    {
        int I = index[i];
        for (int k = i; k < N ; k++)
        {
            if (I+k-i == start)
            {
                startI = i; startJ = k;
                
                k = N;i = N;
            }
        }
    }

    int temp = 0, j = startJ;
    for (int i = startI; i < N; i++){
        int I = index[i];
        for ( ; j < N; j++){
            // if all pairs are calculated
            if (temp == mission) {goto here;}

            for (int k = 0; k < M; k++){
                //printf("I = %d\t matrix[%d][%d] = %f\t matrix[%d][%d] = %f\n",I,i, k,matrix[i][k], j, k,matrix[j][k]);
                result[I+j-i] += matrix[i][k] * matrix[j][k];
            }
            temp += 1;
        }
        if (j == N)  {j = i+1;}
    }
    here: ;
    return NULL;
}

void *efficiency(void *threading){
    int thread_ID, N, M, B, singleT_E, extraT_E, partition;
    double **matrix, *result;
    int *index, *indexOfBlocks;

    struct thread_data *Mydata;

    Mydata = (struct thread_data *)threading;
    thread_ID = Mydata -> thread_ID;
    matrix = Mydata -> matrix;
    index = Mydata -> index;
    indexOfBlocks = Mydata -> indexOfBlock;
    result = Mydata -> result;
    singleT_E = Mydata -> singleT_E;
    extraT_E = Mydata -> extraT_E;
    M = Mydata -> M;
    N = Mydata -> N;
    B = Mydata -> B;
    partition = Mydata -> partition;
    //sleep(thread_ID);

    int startID_A, startID_At, start, task;

    // Calculate the number of tasks and the position of the initial block.
    // If ID smaller than the extraT, it means that this thread has more mission.
    if (thread_ID < extraT_E)
    {
        start = thread_ID * (singleT_E + 1);
        task = singleT_E + 1;
    } else{
        start = thread_ID * singleT_E + extraT_E;
        task = singleT_E;
    }

    // The situation that thread have no task.
    if (task == 0 )
    {
        return NULL;
    }

    //Calculate subscripts of the partition.
    for (int i = 0; i < partition; i++)
    {
        int I = indexOfBlocks[i];
        for (int j = 0; j < partition; j++)
        {
            if (I+j-i == start)
            {
                startID_A = i; startID_At = j;
                j = partition; i = partition;
            }
        }
    }


    int temp = 0, blockID_At = startID_At;
    // Matches A and AT to form the block
    for (int blockID_A = startID_A; blockID_A < partition; blockID_A++)
    {
        for ( ; blockID_At < partition; blockID_At++)
        {
            if (temp == task)
            {
                return NULL;
            }
            if (blockID_A > blockID_At)
            {
                break;
            }
            // calculate in the block
            for (int IA = blockID_A * B; IA < (blockID_A+1) * B && IA< N; IA++)
            {
                int I = index[IA];
                for (int JAt = blockID_At * B; JAt < (blockID_At+1) * B && JAt<N; JAt++)
                {
                    if (IA > JAt)
                    {
                        continue;
                    }
                    double sum = 0;
                    int flag = 0;
                    if (M & 1)
                    {
                        flag = 1;
                        sum = matrix[IA][M-1] * matrix[JAt][M-1];
                    }
                    for (int k = 0; k < M - flag; k+=2)
                    {
                        sum += matrix[IA][k] * matrix[JAt][k];
                        sum += matrix[IA][k+1] * matrix[JAt][k+1];
                    }
                    result[I + JAt - IA] = sum;
                }
            }
            temp += 1;
        }

        // Reset AT
        if (blockID_At == partition)
        {
            blockID_At = blockID_A +1;
        }

    }
    //here: ;

    return NULL;
}


void checkResult(double **matrix, int *index, double *check, double *result, int N, int M);

int main(int argc, char *argv[]){
    int N, M, T, B;

    if (argc == 5)
    {
        N = atoi(argv[1]);
        M = atoi(argv[2]);
        T = atoi(argv[3]);
        B = atoi(argv[4]);
    } 
    //else if(argc == 4){
    //    N = atoi(argv[1]);
    //    M = atoi(argv[2]);
    //    T = atoi(argv[3]);
    //    B = T;
    //} 
    else{
        printf("Usage: %s N M T B\n\n"
               " N: matrix A row length\n"
               " M: matrix A column length\n"
               " T: the number of threads\n"
               " B: the block size\n\n",argv[0]);
        return 1;
    }
    printf("N=%d  M=%d  T=%d  B=%d\n", N, M, T, B);

    if (T == 0 || B == 0 || N == 0 || M == 0)
    {
        printf("Unexpected input");
        return 0;
    }
    

    pthread_t thread[T];
    struct thread_data data_array[T];
    struct timeval start_time, end_time;

    double* matrix0;
    double** matrix;//original matrix
    double* result;// result matrix
    int* index;//index of the result matrix

    // A one-dimensional matrix containing the starting position of each row of the block
    int* indexOfBlock;

    double* check;

    // The number of pairs
    int pairs = N*(N+1)/2;

    //The average number of pairs to be executed per thread in the baseline algorithm
    int singleT_B = pairs/T;
    //The number of threads in the baseline algorithm to execute one more pair
    int extraT_B = pairs/T;
    
    // N divid by block size
    int partition = ceil((double)N/(double)B);
    // The number of blocks
    int blockNum = partition*(partition+1)/2;
    //The average number of blocks is computed per thread in the efficient algorithm.
    int singleT_E = blockNum/T;
    //The number of threads in the efficient algorithm should compute one more block.
    int extraT_E = blockNum %T;
    

    // initialize the original matrix
    matrix0 = (double*)malloc(N*M*sizeof(double));
    matrix = (double**)malloc(N*sizeof(double*));
    for (int i = 0; i < N; i++)
    {
        matrix[i] = matrix0 + i * M;
    }
    srand(time(0));
    for (int i = 0; i < N; i++){
        for (int j = 0; j < M; j++){
            matrix[i][j] = (double)rand()/RAND_MAX;
        }
    }

    // set the result matrix, check matrix and index matrix
    result = (double*)malloc(pairs*sizeof(double));
    index = (int*)malloc(N*sizeof(int));
    for (int i = 0; i < N; i++){
        if (i == 0){
            index[0] = 0;
        } else {
            index[i] = index[i-1] + N-i+1;
        }
    }

    check = (double*)malloc(pairs*sizeof(double));

    indexOfBlock = (int*)malloc(partition*sizeof(int));
        for (int i = 0; i < partition; i++){
            if (i == 0){
                indexOfBlock[0] = 0;
            } else {
                indexOfBlock[i] = indexOfBlock[i-1] + partition-i+1;
            }
        }

    //baseline
    gettimeofday(&start_time, 0);
    for (int i = 0; i < T; i++)
    {
        //printf("create thread %d\n",i);
        data_array[i].thread_ID = i;
        data_array[i].matrix = matrix;
        data_array[i].index = index;
        data_array[i].result = result;
        data_array[i].N = N;
        data_array[i].M = M;
        data_array[i].singleT_B = singleT_B;
        data_array[i].extraT_B = extraT_B;
        pthread_create(&thread[i], NULL, baseline, &data_array[i]);
    }
    for (int i = 0; i < T; i++)
    {
        pthread_join(thread[i], NULL);
    }
    gettimeofday(&end_time, 0);
    long seconds = end_time.tv_sec - start_time.tv_sec;
    long microseconds = end_time.tv_usec - start_time.tv_usec;
    double elapsed = seconds + microseconds/1000000.0;
    printf("\nbaseline tooks %8f seconds to complete. \n", elapsed);

    //efficiency
    gettimeofday(&start_time, 0);
    for (int t = 0; t < T; t++)
    {
        data_array[t].thread_ID = t;
        data_array[t].matrix = matrix;
        data_array[t].result = result;
        data_array[t].N = N;
        data_array[t].M = M;
        data_array[t].B = B;
        data_array[t].index = index;
        data_array[t].indexOfBlock = indexOfBlock;
        data_array[t].partition = partition;
        data_array[t].singleT_E = singleT_E;
        data_array[t].extraT_E = extraT_E;
        pthread_create(&thread[t], NULL, efficiency, &data_array[t]);
    }
    for (int i = 0; i < T; i++)
    {
        pthread_join(thread[i], NULL);
    }
    gettimeofday(&end_time, 0);


    seconds = end_time.tv_sec - start_time.tv_sec;
    microseconds = end_time.tv_usec - start_time.tv_usec;
    elapsed = seconds + microseconds/1000000.0;
    printf("\nefficiency tooks %8f seconds to complete. \n", elapsed);

    //check the result
    checkResult(matrix, index, check, result, N, M);
    return 0;
}

void checkResult(double **matrix, int *index, double *check, double *result, int N, int M){
    for (int i = 0; i < N; i++){
        for (int j = i; j < N; j++){
            for (int  k = 0; k < M; k++){
                check[index[i]+j-i] += matrix[i][k] * matrix[j][k];
            }
            if (fabs(check[index[i]+j-i] - result[index[i]+j-i]) > 0.000000001)
            {
                printf("--different number-- \nresult[%d] C[%d][%d]= %f, right answer: %f\n",index[i]+j-i, i, j, result[index[i]+j-i], check[index[i]+j-i]);
                continue;
            }
        }
    }
}

