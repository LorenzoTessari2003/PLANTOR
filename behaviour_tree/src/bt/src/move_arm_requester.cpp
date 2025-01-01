#include "move_arm_requester.hpp"

MoveArmRequester::MoveArmRequester( 
  const std::string& name, 
  const BT::NodeConfig& config,
  const BT::RosNodeParams& params
)
: BT::RosServiceNode<GoToPose>(name, config, params) 
{}


bool 
MoveArmRequester::setRequest(
  GoToPose::Request::SharedPtr & request
) 
{
  try{
    request->pose.position.x = getInput<double>("x").value();
    request->pose.position.y = getInput<double>("y").value();
    request->pose.position.z = getInput<double>("z").value();
    double R = getInput<double>("R").value();
    double P = getInput<double>("P").value();
    double Y = getInput<double>("Y").value();
    tf2::Quaternion q;
    q.setRPY(R, P, Y);
    q.normalize();
    request->pose.orientation = tf2::toMsg(q);
    return true;
  }
  catch (...) {
    return false;
  }
}


BT::NodeStatus 
MoveArmRequester::onResponseReceived(
  const GoToPose::Response::SharedPtr & response
) 
{
  std::shared_ptr<rclcpp::Node> node = this->node_.lock();
  if(response->result){
    return BT::NodeStatus::SUCCESS;
  }
  else {
    return BT::NodeStatus::FAILURE;
  }
}

 
BT::NodeStatus 
MoveArmRequester::onFailure(
  BT::ServiceNodeErrorCode error
) 
{
  // Apparently weak_ptr does not have a -> operator, so one has to lock it to retrieve the shared_ptr
  std::shared_ptr<rclcpp::Node> node = this->node_.lock();
  if (node){
    RCLCPP_ERROR(node->get_logger(), "Service failed with error code %s", BT::toStr(error));
  }
  return BT::NodeStatus::FAILURE;
}