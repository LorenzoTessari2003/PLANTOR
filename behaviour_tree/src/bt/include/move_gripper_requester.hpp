#pragma once

#include "behaviortree_ros2/bt_service_node.hpp"

#include "rclcpp/rclcpp.hpp"
#include "std_srvs/srv/set_bool.hpp"

using BoolSrv = std_srvs::srv::SetBool;


/**
 * @brief MoveGripperRequester is a class that sends a request of type std_srvs::srv::SetBool to a service and returns the status of the request
 * 
 * In the XML one can set:
 * - service_name: name of the service to call
 * - trigger: if "true" then the request will be true, otherwise false 
 */
class MoveGripperRequester : public BT::RosServiceNode<BoolSrv>
{
public:
  MoveGripperRequester( const std::string& name, 
                        const BT::NodeConfig& config,
                        const BT::RosNodeParams& params);

  static BT::PortsList providedPorts()
  {
    return { 
      BT::InputPort<std::string>("service_name"),
      BT::InputPort<std::string>("trigger") 
    };
  }

  bool setRequest(BoolSrv::Request::SharedPtr & request) override;

  BT::NodeStatus onResponseReceived(const BoolSrv::Response::SharedPtr & response) override;

  virtual BT::NodeStatus onFailure(BT::ServiceNodeErrorCode error) override;
};