#include <opencv2/highgui.hpp>
#include <opencv2/aruco.hpp>
#include <opencv2/imgproc/imgproc.hpp>
#include <iostream>

std::string dumpCornersAsJSON(std::vector<std::vector<cv::Point2f>> markers_corners, std::vector<int> ids) {
    std::string json("[");
    for (int i = 0; i < markers_corners.size(); i++) {
        auto corners = markers_corners[i];
        int marker_id = ids[i];
       
        json += cv::format("{\"%d\":[", marker_id);
        for (int j = 0; j < corners.size() - 1; j++) {
            json += cv::format("{\"x\":%.0f,\"y\":%.0f}, ", corners[j].x, corners[j].y);
        }
        json += cv::format("{\"x\":%.0f,\"y\":%.0f}", corners[corners.size() - 1].x, corners[corners.size() - 1].y);
        json += "]}";
    }
    json += "]";
    return json;
}

int main() {
    cv::VideoCapture capture(0);
    auto dictionary = cv::aruco::getPredefinedDictionary(cv::aruco::DICT_6X6_250);
    auto parameters = cv::aruco::DetectorParameters::create();
    while (capture.grab()) {
        cv::Mat input_image;
        capture.retrieve(input_image);
        std::vector<int> markers_ids;
        std::vector<std::vector<cv::Point2f>> markers_corners, rejected_markers;
        cv::aruco::detectMarkers(input_image, dictionary, markers_corners, markers_ids, parameters, rejected_markers);
        cv::aruco::drawDetectedMarkers(input_image, markers_corners, markers_ids);
        for (int i = 0; i < markers_corners.size(); i++) {
            std::cout << "Corners: " << dumpCornersAsJSON(markers_corners, markers_ids) << std::endl; 
        }

        imshow("markers", input_image);
        if (cv::waitKey(10) == 27) break;
    }
    capture.release();
    return 0;
}
