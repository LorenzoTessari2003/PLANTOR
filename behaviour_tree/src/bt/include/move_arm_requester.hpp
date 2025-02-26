#pragma once

#include "behaviortree_ros2/bt_service_node.hpp"

#include "rclcpp/rclcpp.hpp"
#include "bt_interfaces/srv/go_to_pose.hpp"
#include <tf2/LinearMath/Quaternion.h>
#include <tf2_geometry_msgs/tf2_geometry_msgs.h>

using GoToPose = bt_interfaces::srv::GoToPose;


/**
 * @brief MoveArmRequester is a class that sends a request of type bt_interfaces::srv::GoToPose to a service and returns the status of the request
 * 
 * In the XML one can set:
 * - service_name: name of the service to call
 * - trigger: if "true" then the request will be true, otherwise false 
 */
class MoveArmRequester : public BT::RosServiceNode<GoToPose>
{
public:
  MoveArmRequester( const std::string& name, 
                        const BT::NodeConfig& config,
                        const BT::RosNodeParams& params);

  static BT::PortsList providedPorts()
  {
    return { 
      BT::InputPort<std::string>("service_name"),
      BT::InputPort<double>("x"), 
      BT::InputPort<double>("y"), 
      BT::InputPort<double>("z"),
      BT::InputPort<double>("R"),
      BT::InputPort<double>("P"),
      BT::InputPort<double>("Y")
    };
  }

  bool setRequest(GoToPose::Request::SharedPtr & request) override;

  BT::NodeStatus onResponseReceived(const GoToPose::Response::SharedPtr & response) override;

  virtual BT::NodeStatus onFailure(BT::ServiceNodeErrorCode error) override;
};