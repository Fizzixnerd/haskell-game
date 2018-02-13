#include <btBulletDynamicsCommon.h>
#include <btBulletCollisionCommon.h>

#include "cbullet.h"

extern "C" {

  broadphase_interface* new_broadphase_interface() {
    return reinterpret_cast<broadphase_interface*>(new btDbvtBroadphase());
  }
  
  void free_broadphase_interface(broadphase_interface* broadphase) {
    delete reinterpret_cast<btDbvtBroadphase*>(broadphase);
  }

  collision_configuration* new_default_collision_configuration() {
    return reinterpret_cast<collision_configuration*>(new btDefaultCollisionConfiguration());
  }

  void free_collision_configuration(collision_configuration* collision_configuration) {
    delete reinterpret_cast<btCollisionConfiguration*>(collision_configuration);
  }

  collision_dispatcher* new_collision_dispatcher(collision_configuration* collision_configuration) {
    return reinterpret_cast<collision_dispatcher*>
      (new btCollisionDispatcher
       (reinterpret_cast<btCollisionConfiguration*>
	(collision_configuration)));
  }

  void free_collision_dispatcher(collision_dispatcher* dispatcher) {
    delete reinterpret_cast<btCollisionDispatcher*>(dispatcher);
  }

  constraint_solver* new_sequential_impulse_constraint_solver() {
    return reinterpret_cast<constraint_solver*>(new btSequentialImpulseConstraintSolver());
  }
  
  void free_constraint_solver(constraint_solver* solver) {
    delete reinterpret_cast<btConstraintSolver*>(solver);
  }

  dynamics_world* new_discrete_dynamics_world
  (collision_dispatcher* dispatcher,
   broadphase_interface* broadphase,
   constraint_solver* solver,
   collision_configuration* collision_configuration) {
    return reinterpret_cast<dynamics_world*>(new btDiscreteDynamicsWorld
      (reinterpret_cast<btDispatcher*>(dispatcher),
       reinterpret_cast<btBroadphaseInterface*>(broadphase),
       reinterpret_cast<btConstraintSolver*>(solver),
       reinterpret_cast<btCollisionConfiguration*>(collision_configuration)));
  }

  void free_dynamics_world(dynamics_world* world) {
    delete reinterpret_cast<btDynamicsWorld*>(world);
  }

  void set_gravity(dynamics_world* world, scalar x, scalar y, scalar z) {
    reinterpret_cast<btDynamicsWorld*>(world)->setGravity(btVector3(x, y, z));
  }

  int step_simulation(dynamics_world* world, scalar time_step, const int* max_sub_steps, const scalar* fixed_time_step) {
    int mss = max_sub_steps ? *max_sub_steps : 1;
    scalar fts = fixed_time_step ? *fixed_time_step : (1.0f / 60.0f);
    return reinterpret_cast<btDynamicsWorld*>(world)->
      stepSimulation(time_step, mss, fts);
  }
  
  void add_rigid_body(dynamics_world* world, rigid_body* rigid_body) {
    reinterpret_cast<btDynamicsWorld*>(world)->
      addRigidBody(reinterpret_cast<btRigidBody*>(rigid_body));
  }

  int get_num_collision_objects(dynamics_world* world) {
    return reinterpret_cast<btDynamicsWorld*>(world)->getNumCollisionObjects();
  }

  collision_object* get_collision_object(dynamics_world* world, int idx) {
    return reinterpret_cast<collision_object*>
      (reinterpret_cast<btDynamicsWorld*>(world)->
       getCollisionObjectArray()[idx]);
  }

  void remove_collision_object(dynamics_world* world, collision_object* obj) {
    reinterpret_cast<btDynamicsWorld*>(world)->
      removeCollisionObject(reinterpret_cast<btCollisionObject*>(obj));
      
  }

  int get_num_constraints(dynamics_world* world) {
    return reinterpret_cast<btDynamicsWorld*>(world)->getNumConstraints();
  }
  
  typed_constraint* get_constraint(dynamics_world* world, int idx) {
    return reinterpret_cast<typed_constraint*>
      (reinterpret_cast<btDynamicsWorld*>(world)->getConstraint(idx));
  }

  void add_constraint(dynamics_world* world, typed_constraint* constraint) {
    reinterpret_cast<btDynamicsWorld*>(world)->
      addConstraint(reinterpret_cast<btTypedConstraint*>(constraint));
  }

  void remove_constraint(dynamics_world* world, typed_constraint* constraint) {
    reinterpret_cast<btDynamicsWorld*>(world)->
      removeConstraint(reinterpret_cast<btTypedConstraint*>(constraint));
  }

  void serialize(dynamics_world* world, serializer* serializer) {
    reinterpret_cast<btDynamicsWorld*>(world)->
      serialize(reinterpret_cast<btSerializer*>(serializer));
  }

  // btCollisionObject
  rigid_body* collision_object_to_rigid_body(collision_object* obj) {
    return reinterpret_cast<rigid_body*>
      (btRigidBody::upcast
       (reinterpret_cast<btCollisionObject*>(obj)));
  }

  transform* co_allocate_world_transform(collision_object* obj) {
      btTransform trans = reinterpret_cast<btCollisionObject*>(obj)->
	getWorldTransform();
      btTransform* pTrans = new btTransform(trans);
      return reinterpret_cast<transform*>(pTrans);
  }

  // btMotionState
  motion_state* new_default_motion_state(transform* transform) {
    return reinterpret_cast<motion_state*>
      (new btDefaultMotionState(*reinterpret_cast<btTransform*>(transform)));
  }

  void free_motion_state(motion_state* motion_state) {
    delete reinterpret_cast<btMotionState*>(motion_state);
  }

  transform* ms_allocate_world_transform(motion_state* motion_state) {
    btTransform* trans = new btTransform;
    reinterpret_cast<btMotionState*>(motion_state)->getWorldTransform(*trans);
    return reinterpret_cast<transform*>(trans);
  }

  // btRigidBodyConstructioninfo
  rigid_body_construction_info* new_rigid_body_construction_info
  (scalar mass,
   motion_state* motion_state,
   collision_shape* collision_shape,
   scalar inertia_x,
   scalar inertia_y,
   scalar inertia_z) {
    return reinterpret_cast<rigid_body_construction_info*>
      (new btRigidBody::btRigidBodyConstructionInfo
       (mass,
	reinterpret_cast<btMotionState*>(motion_state),
	reinterpret_cast<btCollisionShape*>(collision_shape),
	btVector3(inertia_x, inertia_y, inertia_z)));
  }

  void free_rigid_body_construction_info(rigid_body_construction_info* rbci) {
    delete reinterpret_cast<btRigidBody::btRigidBodyConstructionInfo*>(rbci);
  }

  // btRigidbody
  rigid_body* new_rigid_body(rigid_body_construction_info* rbci) {
    btRigidBody* rb = new btRigidBody
      (*reinterpret_cast<btRigidBody::btRigidBodyConstructionInfo*>(rbci));
    return reinterpret_cast<rigid_body*>(rb);
  }

  void free_rigid_body(rigid_body* rigid_body) {
    delete reinterpret_cast<btRigidBody*>(rigid_body);
  }

  motion_state* rb_get_motion_state(rigid_body* rigid_body) {
    return reinterpret_cast<motion_state*>
      (reinterpret_cast<btRigidBody*>(rigid_body)->
       getMotionState());
  }

  int is_static_object(rigid_body* rigid_body) {
    return reinterpret_cast<btRigidBody*>(rigid_body)->
      isStaticObject();
  }

  int is_kinematic_object(rigid_body* rigid_body) {
    return reinterpret_cast<btRigidBody*>(rigid_body)->
      isKinematicObject();
  }

  void set_activation_state(rigid_body* rigid_body, int new_state) {
    reinterpret_cast<btRigidBody*>(rigid_body)->setActivationState(new_state);
  }

  transform* get_center_of_mass_transform(rigid_body* rigid_body) {
    return reinterpret_cast<transform*>
      (new btTransform
       (reinterpret_cast<btRigidBody*>(rigid_body)->
	getCenterOfMassTransform()));
  }

  // btCollisionShape
  void calculate_local_inertia(collision_shape* collision_shape,
			       scalar mass,
			       scalar* x_out,
			       scalar* y_out,
			       scalar* z_out) {
    btVector3 inertia;
    reinterpret_cast<btCollisionShape*>(collision_shape)->
      calculateLocalInertia(mass, inertia);
    *x_out = inertia[0];
    *y_out = inertia[1];
    *z_out = inertia[2];
  }

  void free_collision_shape(collision_shape* collision_shape) {
    delete reinterpret_cast<btCollisionShape*>(collision_shape);
  }

  // int default_step_simulation(discrete_dynamics_world* world, scalar time_step) {
  //   return step_simulation(world, time_step, NULL, NULL);
  // }

  // btStaticPlaneShape
  static_plane_shape* new_static_plane_shape(scalar x,
					     scalar y,
					     scalar z,
					     scalar plane_constant) {
    return reinterpret_cast<static_plane_shape*>(new btStaticPlaneShape(btVector3(x, y ,z), plane_constant));
  }
  
  void free_static_plane_shape(static_plane_shape* static_plane_shape) {
    delete reinterpret_cast<btStaticPlaneShape*>(static_plane_shape);
  }
  
  // btSphereShape
  sphere_shape* new_sphere_shape(scalar radius) {
    return reinterpret_cast<sphere_shape*>(new btSphereShape(radius));
  }

  void free_sphere_shape(sphere_shape* sphere_shape) {
    delete reinterpret_cast<btSphereShape*>(sphere_shape);
  }
  
  //btBoxShape
  box_shape* new_box_shape(scalar half_x, scalar half_y, scalar half_z) {
    return reinterpret_cast<box_shape*>(new btBoxShape(btVector3(half_x, half_y, half_z)));
  }

  void free_box_shape(box_shape* box_shape) {
    delete reinterpret_cast<btBoxShape*>(box_shape);
  }
  
  // btTransform
  transform* new_transform(scalar r, // quaternion
			   scalar i,
			   scalar j,
			   scalar k,
			   scalar x, // vector
			   scalar y,
			   scalar z) {
    return reinterpret_cast<transform*>
      (new btTransform(btQuaternion(r, i, j, k), btVector3(x, y, z)));
  }

  void free_transform(transform* transform) {
    delete reinterpret_cast<btTransform*>(transform);
  }

  void get_origin(transform* transform, scalar* x_out, scalar* y_out, scalar* z_out) {
    btVector3 vec = reinterpret_cast<btTransform*>(transform)->getOrigin();
    *x_out = vec[0];
    *y_out = vec[1];
    *z_out = vec[2];
  }

  void set_origin(transform* transform, scalar x, scalar y, scalar z) {
    reinterpret_cast<btTransform*>(transform)->setOrigin(btVector3(x, y, z));
  }

  void set_identity(transform* transform) {
    reinterpret_cast<btTransform*>(transform)->setIdentity();
  }

  // btTypedConstraint
  void free_typed_constraint(typed_constraint* constraint) {
    delete reinterpret_cast<btTypedConstraint*>(constraint);
  }

  // btPoint2PointConstraint
  typed_constraint* new_point2point_constraint(rigid_body* rigid_body,
					       scalar pivot_x,
					       scalar pivot_y,
					       scalar pivot_z) {
    return reinterpret_cast<typed_constraint*>
      (new btPoint2PointConstraint
       (*reinterpret_cast<btRigidBody*>(rigid_body),
	btVector3(pivot_x, pivot_y, pivot_z)));
  }

  // btDefaultSerializer
  serializer* new_default_serializer() {
    return reinterpret_cast<serializer*>(new btDefaultSerializer());
  }
  
  void free_serializer(serializer* serializer) {
    delete reinterpret_cast<btSerializer*>(serializer);
  }

  const unsigned char* get_buffer_pointer(serializer* serializer) {
    return reinterpret_cast<btSerializer*>(serializer)->getBufferPointer();
  }

  int get_current_buffer_size(serializer* serializer) {
    return reinterpret_cast<btSerializer*>(serializer)->getCurrentBufferSize();
  }

}
