#include <iostream>
#include <opencv2/core/core.hpp>
#include <opencv2/highgui/highgui.hpp>
#include <opencv2/imgproc.hpp>
#include <opencv2/dnn/dnn.hpp>


const int POINTS_COUNT = 22;
const int PAIRS_COUNT = 20;
const int HAND_MARKS_PAIRS[20][2] = {
	{0,1}, {1,2}, {2,3}, {3,4},         // thumb
	{0,5}, {5,6}, {6,7}, {7,8},         // pinkie
	{0,9}, {9,10}, {10,11}, {11,12},    // middle
	{0,13}, {13,14}, {14,15}, {15,16},  // ring
	{0,17}, {17,18}, {18,19}, {19,20}   // small
};

cv::dnn::Net getNet() {
	return cv::dnn::readNet("models/pose_iter_102000.caffemodel", "models/pose_deploy.prototxt");
}

std::vector<cv::Point> getPoints(const cv::Mat& image, cv::dnn::Net& net, float threshold = 0.1) {
	const int blob_width = 368, blob_height =368;
	const float blob_scale = 0.003922;
	auto blob = cv::dnn::blobFromImage(image, blob_scale, cv::Size(blob_width, blob_height), cv::Scalar(0, 0, 0));
	net.setInput(blob);
	auto result = net.forward();
	int rows = result.size[2], cols = result.size[3];

	float x_scale_ratio = float(image.cols) / cols;
	float y_scale_ratio = float(image.rows) / rows;

	std::vector<cv::Point> points(POINTS_COUNT);
	for (int i = 0; i < POINTS_COUNT; i++) {
		cv::Mat heat_map(rows, cols, CV_32F, result.ptr(0, i));
		cv::Point point(-1, -1), temp_point;
		double confidence;
		cv::minMaxLoc(heat_map, 0, &confidence, 0, &temp_point);
		if (confidence >= threshold) point = temp_point;
		point.x *= x_scale_ratio; point.y *= y_scale_ratio;
		points[i] = point;
	}
	return points;
}

std::string dumpPointAsJSON(cv::Point point) {
	return "{\"x\":" + cv::format("%d", point.x) + ",\"y\":" + cv::format("%d", point.y) + "}";
}

std::string dumpPointsAsJSON(std::vector<cv::Point> points) {
	std::string json = "[";
	for (int i = 0; i < points.size() - 1; i++) {
		json.append(dumpPointAsJSON(points[i]) + ",");
	}
	json.append(dumpPointAsJSON(points[points.size() - 1]) + "]");
	return json;
}

void draw(cv::Mat& image, std::vector<cv::Point> points) {
	for (int i = 0; i < PAIRS_COUNT; i++) {
		cv::Point2f point_a = points[HAND_MARKS_PAIRS[i][0]];
		cv::Point2f point_b = points[HAND_MARKS_PAIRS[i][1]];
		if (point_a.x <= 0 || point_a.y <= 0 || point_b.x <= 0 || point_b.y <= 0) continue;


		cv::line(image, point_a, point_b, cv::Scalar(102, 111, 237), 3);
		cv::circle(image, point_a, 5, cv::Scalar(102, 217, 237), -1);
		cv::circle(image, point_b, 5, cv::Scalar(102, 217, 237), -1);
	}

	for (int i = 0; i < POINTS_COUNT; i++) {
		cv::putText(image, cv::format("(%d; %d)", points[i].x, points[i].y), points[i], cv::FONT_HERSHEY_COMPLEX, .5, cv::Scalar(0, 0, 0), 1);
	}
}

int main()
{
	auto net = getNet();
	cv::VideoCapture camera(0);
	if (!camera.isOpened()) {
		std::cerr << "ERROR: Could not open camera" << std::endl;
		return 1;
	}

	cv::namedWindow("Hands");
	while (1) {
		cv::Mat image;
		camera >> image;
		auto points = getPoints(image, net, 0.000001);
		std::cout << dumpPointsAsJSON(points) << std::endl;
		draw(image, points);
		cv::imshow("Hands", image);
		if (cv::waitKey(10) == 27) break;
	}
	
	
	return 0;
}
