#define loop_name(i) loop##i

#ifdef XFRP_ON_PTHREAD
#include <pthread.h>
pthread_t th[1];
#define fork(i) pthread_create(th[i], NULL, loop_name(i), NULL)
pthread_barrier_t barrier;
#define init_barrier(thread) pthread_barrier_init(&barrier,NULL,(thread))
#define synchronization(tid) pthread_barrier_wait(&barrier);
#endif

#ifdef XFRP_ON_ESP32
#include <Arduino.h>
#include <M5Stack.h>
#define TASK0_BIT (1 << 0)
#define ALL_TASK_BIT (TASK0_BIT)
#define fork(i) xTaskCreatePinnedToCore(loop_name(i),"Task##i",8192,NULL,1,NULL,0)
EventGroupHandle_t barrier;
#define init_barrier(thread) barrier = xEventGroupCreate();
#define synchronization(i) xEventGroupSync(barrier,TASK ## i ## _BIT ,ALL_TASK_BIT,portMAX_DELAY);
#endif

#include "opencv2/opencv.hpp"
#include <iostream>
#include <cstdint>
#include <chrono>

cv::VideoCapture cap(0);
int h,w;

std::chrono::system_clock::time_point start,end;

void user_setup(){
	std::cout << "setup" << std::endl;
	if(!cap.isOpened()){
		std::cerr << "VideoCapture not opened" << std::endl;
		return;
	}
}

void input(int image[]){
	//std::cout << "input()" << std::endl;
	cv::Mat frame;
	cap.read(frame);
	h = frame.rows / 2;
	w = frame.cols / 2;
	for(int x=0;x<h;x++){
		cv::Vec3b *ptr = frame.ptr<cv::Vec3b>(x);
		for(int y=0;y<w;y++){
			int R = ptr[y][0];
			int G = ptr[y][1];
			int B = ptr[y][2];
			image[x*w+y] = (R<<16) | (G<<8) | B;
		}
	}
	start = std::chrono::system_clock::now();
}

void output(float grayImage[]){
	end = std::chrono::system_clock::now();
	double elapsed = std::chrono::duration_cast<std::chrono::milliseconds>(end-start).count();
	printf("%lf, ",elapsed);
	//std::cout << grayImage[0] << std::endl;
	cv::Mat out(h,w,CV_32F,grayImage);
	cv::imshow("gray", out);
	const int key = cv::waitKey(1);
	if(key == 'q'){
		exit(0);
	}
}
