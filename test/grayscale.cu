#include<stdio.h>
#include<stdlib.h>
#include<cuda_runtime.h>
#include<helper_functions.h>
#include<helper_cuda.h>

#include "setting.h"

#define bool int
#define true 1
#define false 0

int turn = 0;
int image[2][1228800];
int* g_image[2];
float grayImage[2][1228800];
int red[2][1228800];
int* g_red[2];
int green[2][1228800];
int* g_green[2];
int blue[2][1228800];
int* g_blue[2];
float red_rg[2][1228800];
float* g_red_rg[2];
float green_rg[2][1228800];
float* g_green_rg[2];
float blue_rg[2][1228800];
float* g_blue_rg[2];
float grayscale_rg[2][1228800];
float* g_grayscale_rg[2];
float grayscale[2][1228800];
float* g_grayscale[2];

void grayImage_update(int self){
	float tmp_0;
	bool tmp_1 = (self < 1228800);
	if(!!tmp_1){
		tmp_0 = grayscale[turn][self];
	}else{
		tmp_0 = 0.5;
	}
	grayImage[turn][self] = tmp_0;
}

__global__ void red_kernel(int* red, int* image){
	int self = blockIdx.x * blockDim.x + threadIdx.x;
	if(self < 1228800){
		red[self] = ((image[self] >> 16) & 255);
	}
}
void red_update(){
	red_kernel<<<dim3(2400),dim3(512)>>>(g_red[turn], g_image[turn]);
}

__global__ void green_kernel(int* green, int* image){
	int self = blockIdx.x * blockDim.x + threadIdx.x;
	if(self < 1228800){
		green[self] = ((image[self] >> 8) & 255);
	}
}
void green_update(){
	green_kernel<<<dim3(2400),dim3(512)>>>(g_green[turn], g_image[turn]);
}

__global__ void blue_kernel(int* blue, int* image){
	int self = blockIdx.x * blockDim.x + threadIdx.x;
	if(self < 1228800){
		blue[self] = (image[self] & 255);
	}
}
void blue_update(){
	blue_kernel<<<dim3(2400),dim3(512)>>>(g_blue[turn], g_image[turn]);
}

__global__ void red_rg_kernel(float* red_rg, int* red){
	int self = blockIdx.x * blockDim.x + threadIdx.x;
	if(self < 1228800){
		red_rg[self] = pow((red[self] / 255.),2.2);
	}
}
void red_rg_update(){
	red_rg_kernel<<<dim3(2400),dim3(512)>>>(g_red_rg[turn], g_red[turn]);
}

__global__ void green_rg_kernel(float* green_rg, int* green){
	int self = blockIdx.x * blockDim.x + threadIdx.x;
	if(self < 1228800){
		green_rg[self] = pow((green[self] / 255.),2.2);
	}
}
void green_rg_update(){
	green_rg_kernel<<<dim3(2400),dim3(512)>>>(g_green_rg[turn], g_green[turn]);
}

__global__ void blue_rg_kernel(float* blue_rg, int* blue){
	int self = blockIdx.x * blockDim.x + threadIdx.x;
	if(self < 1228800){
		blue_rg[self] = pow((blue[self] / 255.),2.2);
	}
}
void blue_rg_update(){
	blue_rg_kernel<<<dim3(2400),dim3(512)>>>(g_blue_rg[turn], g_blue[turn]);
}

__global__ void grayscale_rg_kernel(float* grayscale_rg, float* red_rg, float* green_rg, float* blue_rg){
	int self = blockIdx.x * blockDim.x + threadIdx.x;
	if(self < 1228800){
		grayscale_rg[self] = (((0.2126 * red_rg[self]) + (0.7152 * green_rg[self])) + (0.0722 * blue_rg[self]));
	}
}
void grayscale_rg_update(){
	grayscale_rg_kernel<<<dim3(2400),dim3(512)>>>(g_grayscale_rg[turn], g_red_rg[turn], g_green_rg[turn], g_blue_rg[turn]);
}

__global__ void grayscale_kernel(float* grayscale, float* grayscale_rg){
	int self = blockIdx.x * blockDim.x + threadIdx.x;
	if(self < 1228800){
		grayscale[self] = pow(grayscale_rg[self],(1. / 2.2));
	}
}
void grayscale_update(){
	grayscale_kernel<<<dim3(2400),dim3(512)>>>(g_grayscale[turn], g_grayscale_rg[turn]);
	cudaMemcpy(grayscale[turn],g_grayscale[turn],1228800 * sizeof(float), cudaMemcpyDeviceToHost);
}

void update_0_0(){
	for(int i=0;i<1228800;i++) grayImage_update(i);
}
void update_0_1(){
	grayscale_update();
}
void update_0_2(){
	grayscale_rg_update();
}
void update_0_3(){
	blue_rg_update();
	red_rg_update();
	green_rg_update();
}
void update_0_4(){
	red_update();
	green_update();
	blue_update();
}

void loop(){
	update_0_4();
	update_0_3();
	update_0_2();
	update_0_1();
	update_0_0();
}

void input_support(){
	cudaMemcpy(g_image[turn], image[turn], 1228800 * sizeof(int), cudaMemcpyHostToDevice);
}

void setup(){
	turn=0;
	// Input image
	for(int i=0;i<2;i++) cudaMalloc((void**)&g_image[i],1228800*sizeof(int));
	
	/* GPU red */
	for(int i=0;i<2;i++) cudaMalloc((void**)&g_red[i],1228800*sizeof(int));
	
	/* GPU green */
	for(int i=0;i<2;i++) cudaMalloc((void**)&g_green[i],1228800*sizeof(int));
	
	/* GPU blue */
	for(int i=0;i<2;i++) cudaMalloc((void**)&g_blue[i],1228800*sizeof(int));
	
	/* GPU red_rg */
	for(int i=0;i<2;i++) cudaMalloc((void**)&g_red_rg[i],1228800*sizeof(float));
	
	/* GPU green_rg */
	for(int i=0;i<2;i++) cudaMalloc((void**)&g_green_rg[i],1228800*sizeof(float));
	
	/* GPU blue_rg */
	for(int i=0;i<2;i++) cudaMalloc((void**)&g_blue_rg[i],1228800*sizeof(float));
	
	/* GPU grayscale_rg */
	for(int i=0;i<2;i++) cudaMalloc((void**)&g_grayscale_rg[i],1228800*sizeof(float));
	
	/* GPU grayscale */
	for(int i=0;i<2;i++) cudaMalloc((void**)&g_grayscale[i],1228800*sizeof(float));
}

int main(){
	setup();
	user_setup();
	while(1){
		input(image[turn]);
		input_support();
		loop();
		output(grayImage[turn]);
		turn^=1;
	}
}
