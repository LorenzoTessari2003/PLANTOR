cmake_minimum_required(VERSION 3.20)
project(bt)

# message(FATAL_ERROR "${CMAKE_CURRENT_BINARY_DIR}")

set(behaviortree_cpp_DIR "${CMAKE_CURRENT_BINARY_DIR}/../behaviourtree_cpp/ament_cmake_core")

find_package(behaviortree_cpp REQUIRED)
find_package(behaviortree_ros2 REQUIRED)

find_package(ament_cmake REQUIRED)
find_package(rclcpp REQUIRED)
find_package(std_srvs REQUIRED)
find_package(bt_interfaces REQUIRED)
find_package(tf2 REQUIRED)
find_package(tf2_geometry_msgs REQUIRED)

file(GLOB SRC_FILES "src/*.cpp")

add_executable(behaviour_tree ${SRC_FILES})

target_compile_features(behaviour_tree PUBLIC cxx_std_17)

target_include_directories(behaviour_tree PRIVATE 
    include/
)

ament_target_dependencies(behaviour_tree 
    behaviortree_cpp
    behaviortree_ros2
    rclcpp
    std_srvs
    bt_interfaces
    tf2
    tf2_geometry_msgs
)

install(TARGETS 
    behaviour_tree 
    DESTINATION lib/${PROJECT_NAME}
)

ament_package()