#define XML_FILE_PATH "/home/enrico/Projects/prolog_planner/behaviour_tree/src/my_tree.xml"

#include <fstream>
#include <chrono>

#include "behaviortree_cpp/bt_factory.h"
#include "behaviortree_ros2/bt_service_node.hpp"

#include "rclcpp/rclcpp.hpp"

#include "service_requester.hpp"
#include "move_arm_requester.hpp"
#include "move_gripper_requester.hpp"
#include "sleep_node.hpp"

int main(int argc, char * argv[])
{
  std::cout << "argc " << argc << std::endl;
  // Get path of xml_file if passed
  std::string xml_file_path = XML_FILE_PATH;
  if (argc == 2 && std::string(argv[1]) == "-h"){
    std::cout << "Usage: ros2 run behaviour_tree behaviour_tree_node [path_to_xml_file] [other parameters]" << std::endl << std::endl;
    std::cout << "If any ros2 parameters must be passed, then also the path to the XML file must be passed and as the first argument" << std::endl;
    std::cout << "ros2 run behaviour_tree behaviour_tree_node -h to show this message" << std::endl;
    return 0;
  }
  else if (argc > 1){
    // Check if file can be opened
    std::ifstream file(argv[1]);
    if (!file.good()){
      std::cerr << "Error: file " << argv[1] << " not found" << std::endl;
      return 1;
    }
    else {
      file.close();
    }

    // If it can then set it as xml_file_path and remove it from argv
    xml_file_path = argv[1];
    argc --;
    argv = &argv[1];
  }

  // Initialize ROS2 and bt factory
  rclcpp::init(argc, argv);
  BT::BehaviorTreeFactory factory;

  // Register nodes with parameters
  auto move_arm_client_node = std::make_shared<rclcpp::Node>("move_arm_client_node");
  BT::RosNodeParams move_arm_params;
  move_arm_params.nh = move_arm_client_node;
  move_arm_params.wait_for_server_timeout = std::chrono::milliseconds(1000*10);
  move_arm_params.server_timeout = std::chrono::milliseconds(1000*10);
  move_arm_params.default_port_value = "move_arm";
  factory.registerNodeType<MoveArmRequester>("MoveArmRequester", move_arm_params);

  auto move_gripper_client_node = std::make_shared<rclcpp::Node>("move_gripper_client_node");
  BT::RosNodeParams move_gripper_params;
  move_gripper_params.nh = move_gripper_client_node;
  move_gripper_params.wait_for_server_timeout = std::chrono::milliseconds(1000*10);
  move_gripper_params.server_timeout = std::chrono::milliseconds(1000*10);
  move_arm_params.default_port_value = "move_gripper";
  factory.registerNodeType<MoveGripperRequester>("MoveGripperRequester", move_gripper_params);

  factory.registerNodeType<SleepNode>("SleepNode");

  // Create tree from xml file
  auto tree = factory.createTreeFromFile(xml_file_path);

  // Run tree
  tree.tickWhileRunning();

  // Shutdown ROS2
  rclcpp::shutdown();
  return 0;
}
